package scalaclean.rules

import java.io.{PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scalaclean.cli.{SCPatchUtil, ScalaCleanCommandLine}
import scalaclean.model._
import scalaclean.model.impl.{Project, ProjectSet}
import scalaclean.util.{DiffAssertions, DocHelper, PatchStats}
import scalafix.v1.SyntacticDocument

import scala.meta.internal.io.FileIO
import scala.meta.{AbsolutePath}
import scala.reflect.ClassTag

abstract class AbstractRule[T <: ScalaCleanCommandLine] {
  type Rule <: RuleRun[T]
  def cmdLine: T
  def apply(options: T, model: ProjectModel): Rule

  def main(args: Array[String]): Unit = {
    val options = cmdLine
    parse(options, args)

    val projectProps = options.files.map(f => Paths.get(f.toString))

    val projectSet = new ProjectSet(projectProps: _*)

    println(s"Running rule: ${getClass.getSimpleName}")
    apply(options, projectSet).run()

  }

  def parse(cmdLine: ScalaCleanCommandLine, args: Array[String]): Unit = {
    import org.kohsuke.args4j._
    val parser = new CmdLineParser(cmdLine)

    try { // parse the arguments.
      parser.parseArgument(args: _*)
    } catch {
      case e: CmdLineException =>
        System.err.println(e.getMessage)
        System.err.println(s"java ${getClass.getName} [options...] arguments...")
        parser.printUsage(System.err)
        System.err.println()
        System.exit(1)
        ???
    }
  }

}

abstract class RuleRun[T <: ScalaCleanCommandLine] {
  val options: T
  val model: ProjectModel
  def name = getClass.getSimpleName

  val patchStats                              = new PatchStats
  def debug                                   = options.debug
  def addComments                             = options.addComments
  def printSummary(projectName: String): Unit = patchStats.printSummary(projectName)

  type Colour = Mark[SpecificColour]
  type SpecificColour <: SomeSpecificColour
  def dontChange(reason: String)        = Mark.dontChange[SpecificColour](SimpleReason(reason))
  def makeChange(level: SpecificColour) = Mark.specific[SpecificColour](level)

  final def beforeStart(): Unit = {
    if (debug)
      println(s"$name performing analysis")

    markInitial()

    beforeRule()

    if (debug)
      println(s"$name running rule")

    runRule()
    afterRule()

    if (debug)
      debugDump()

    if (debug)
      println(s"$name analysis complete")
  }

  def beforeRule() = {
    if (debug)
      println(s"$name running ${options.rulePlugins} rule plugins before rule")
    options.rulePlugins.foreach { rule =>
      if (debug)
        println(s"$name running rule ${rule.name}  before main rule")
      rule.beforeMainRule(this)
    }
  }

  def afterRule() = {
    if (debug)
      println(s"$name running ${options.rulePlugins} rule plugins after rule")
    options.rulePlugins.foreach { rule =>
      if (debug)
        println(s"$name running rule ${rule.name}  before main rule")
      rule.afterMainRule(this)
    }
  }

  def debugDump(): Unit = {}

  def markInitial(): Unit = markAll[ModelElement](Mark.initial[SpecificColour])

  def runRule(): Unit

  def fix(targetFile: SourceModel, syntacticDocument: () => SyntacticDocument): List[SCPatch]

  def markAll[E <: ModelElement: ClassTag](colour: Colour): Unit = {
    val all = model.allOf[E].toList
    model
      .allOf[E]
      .foreach(
        _.mark = colour
      )
  }

  implicit class Coloured(e: ModelElement) {
    def colour: Colour = e.mark.asInstanceOf[Colour]

    def colour_=(newColour: Colour): Unit = e.mark = newColour
  }

  //utility methods

  def allMainEntryPoints: Iterator[ModelElement] = {
    allMainMethodEntries ++ allApp
  }

  def allMainMethodEntries: Iterator[ModelElement] = {
    model.allOf[ObjectModel].filter(_.isTopLevel).flatMap { om =>
      om.methods.collect { case mm: PlainMethodModel if mm.methodName == "main" => mm }
    }
  }

  def allApp: Iterator[ObjectModel] = {
    val allAppObjects = ElementIds.allAppObjects
    for (obj <- model.allOf[ObjectModel] if allAppObjects.exists(appLike => obj.xtends(appLike)))
      yield obj

  }

  def otherAnnotationBasedEntryPoints: Iterator[ModelElement] = {
    model.allOf[ModelElement].filter { method =>
      method.annotations.exists(a => annotationEntryPoints.contains(a.fqName))
    }
  }

  protected val annotationEntryPoints = Set[String](
    "org.springframework.jmx.export.annotation.ManagedOperation",
    "org.springframework.jmx.export.annotation.ManagedAttribute"
  )

  def allTestEntryPoints: Iterator[ModelElement] = {
    allJunitTest ++ allJunitClasses ++ allScalaTests
  }

  private val junitAnnotationEntryPoints = Set(
    "org.junit.Ignore",
    "org.junit.Test",
    "org.junit.Before",
    "org.junit.After",
    "org.junit.BeforeClass",
    "org.junit.AfterClass",
  )

  def allJunitTest: Iterator[MethodModel] = {
    model.allOf[MethodModel].filter { method =>
      method.annotations.exists(a => junitAnnotationEntryPoints.contains(a.fqName))
    }
  }

  def allJunitClasses: Iterator[ClassLike] = {
    allJunitTest
      .map(_.classOrEnclosing)
      .toSet
      .flatMap { cls: ClassLike => cls.extendedByClassLike() ++ cls.extendsClassLikeCompiled() } iterator
  }

  def allScalaTests: Iterator[ClassLike] = {
    val suite = ElementIds("C:org.scalatest.Suite")
    model.allOf[ClassLike].filter { cls: ClassLike => cls.xtends(suite) }
  }

  def allSerialisationEntries: Iterator[MethodModel] = {
    model.allOf[MethodModel].filter { method =>
      (method.methodName == "writeObject" /*      && method.params == objectOutputStream */ ) ||
      (method.methodName == "readObject" /*       && method.params == objectInputStream */ ) ||
      (method.methodName == "readObjectNoData" /* && method.params == empty */ ) ||
      (method.methodName == "writeReplace" /*     && method.params == empty */ ) ||
      (method.methodName == "readResolve" /*      && method.params == empty */ )
    }
    // ++
  }

  def generateHTML(generated: String, original: String): Unit = {
    import scala.io.Source
    val cssText: String = Source.fromResource("default-style.css").mkString

    writeToFile(Paths.get("/tmp/code.css"), cssText)
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    pw.println(
      """
        |<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
        |<html xmlns="http://www.w3.org/1999/xhtml">
        |    <head>
        |        <title>XXXXX</title>
        |        <link rel="stylesheet" type="text/css" href="code.css" title="Style">
        |    </head>
        |    <body onload="initializeLinked()">
        |        <pre>
        |""".stripMargin
    )

    pw.println(generated)
    pw.println("--------------------")
    pw.println("--------------------")
    pw.println(original)
    pw.println("--------------------")

    pw.println(
      """        </pre>
        |    </body>
        |</html>
        |""".stripMargin
    )

    pw.flush()
    val str = sw.toString
    writeToFile(Paths.get("/tmp/code.html"), str)
    //    Runtime.getRuntime.exec("open /tmp/code.html")
    //    System.exit(1)

  }

  def applyRule(
      targetFile: SourceModel,
      syntacticDocument: () => SyntacticDocument,
      suppress: Boolean,
      source: String,
  ): String = {

    // actually run the rule
    val fixes: Seq[SCPatch] = fix(targetFile, syntacticDocument)
    val fixedSource         = SCPatchUtil.applyFixes(source, fixes)

    //    generateHTML(fixedSource, source)

    fixedSource
  }

  def run(): Boolean = {

    val projectProps = options.files.map(f => Paths.get(f.toString))

    val projectSet = new ProjectSet(projectProps: _*)

    println(s"Running rule: $name")

    beforeStart()

    var changed = false
    projectSet.projects.foreach(project => changed |= runRuleOnProject(project))
    if (options.debug)
      println(s"DEBUG: Changed = $changed")
    changed
  }

  object testSupport extends DiffAssertions {

    def expectedPathForTarget(src: SourceModel): Path = {
      src.filename.resolveSibling(src.filename.getFileName + options.testOptions.expectationSuffix + ".expected")
    }

    def compareAgainstFile(expected: String, obtained: String): Boolean = {

      val diff = DiffAssertions.compareContents(obtained, expected)
      if (diff.nonEmpty) {
        println("###########> obtained       <###########")
        println(obtained)
        println("###########> expected       <###########")
        println(expected)
        println("###########> Diff       <###########")
        println(error2message(obtained, expected))
        true
      } else
        false
    }

  }

  def writeToFile(path: AbsolutePath, content: String): Unit = {
    writeToFile(path.toNIO, content)
  }

  def writeToFile(path: Path, content: String): Unit = {
    Files.write(path, content.getBytes)
  }

  /**
   * @param project The target project
   * @return True if diffs were seen or files were changed
   */
  def runRuleOnProject(
      project: Project
  ): Boolean = {

    var changed = false

    if (debug)
      println("---------------------------------------------------------------------------------------------------")

    val files = model.allOf[SourceModel]

    files.foreach { file =>

      if (!Files.exists(file.filename)) {
        if (options.skipNonexistentFiles)
          patchStats.skippedFile(file.filename)
        else
          throw new IllegalStateException(s"cant find file $file")
      } else {

        val content = new String(Files.readAllBytes(file.filename), StandardCharsets.UTF_8)

        val syntacticDocument = () => DocHelper.readSyntacticDoc(file, content)

        val obtained = applyRule(file, syntacticDocument, suppress = false, content)

        val expectedFile = testSupport.expectedPathForTarget(file)

        def expectedContent = new String(Files.readAllBytes(expectedFile), StandardCharsets.UTF_8)

        if (options.testOptions.validate) {

          changed |= testSupport.compareAgainstFile(expectedContent, obtained)
          if (options.replace) {
            // overwrite the '.expected' file
            writeToFile(expectedFile, obtained)
          }
        } else {
          if (options.replace) {
            // overwrite the base file
            if (debug)
              println(s"DEBUG: Overwriting existing file: $expectedFile")
            writeToFile(expectedFile, obtained)
          } else {

            if (debug)
              println("DEBUG Comparing obtained vs " + expectedFile)

            // diff against original file
            changed |= testSupport.compareAgainstFile(expectedContent, obtained)
          }
        }
      }
    }
    printSummary("ALL")
    changed
  }

}
