package org.scalaclean.analysis

import java.io.File

import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.io.AbsolutePath
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.{Global, Phase}


class ScalaCompilerPluginComponent(val global: Global) extends PluginComponent with SemanticdbOps with ModelSymbolBuilder {
  override val phaseName: String = "scalaclean-compiler-plugin-phase"
  override val runsAfter: List[String] = List("typer") // was typer

  override def newPhase(prev: Phase): Phase = new StdPhase(prev) {

    val debug = false

    var elementsWriter: ElementsWriter = _

    var relationsWriter: RelationshipsWriter = _

    override def run(): Unit = {
      global.reporter.echo("Before Analysis Phase")

      val outputPathBase: AbsolutePath = AbsolutePath(
        global.settings.outputDirs.getSingleOutput
          .flatMap(so => Option(so.file))
          .map(v => v.getAbsolutePath)
          .getOrElse(global.settings.d.value))


      val outputPath = outputPathBase.resolve("META-INF/ScalaClean/")
      outputPath.toFile.mkdirs()
      val elementsFile = new File(outputPath.toFile, "scalaclean-elements.csv")
      println(s"Writing elements file  to ${elementsFile}")
      elementsWriter = new ElementsWriter(elementsFile)

      val relationsFile = new File(outputPath.toFile, "scalaclean-relationships.csv")
      println(s"Writing relationships file to to ${relationsFile}")
      relationsWriter = new RelationshipsWriter(relationsFile, global)


      super.run()
      elementsWriter.finish()
      relationsWriter.finish()
      global.reporter.echo("After Analysis Phase")
      global.reporter.echo(s"  Wrote elements to $elementsFile")
      global.reporter.echo(s"  Wrote relationships to $relationsFile")

    }

    object PlatformPathIO {
      def workingDirectoryString: String =
        sys.props("user.dir")
    }

    override def apply(unit: global.CompilationUnit): Unit = {

      val sourceFile = mungeUnitPath(unit.source.file.toString)
      global.reporter.echo(s"Executing for unit: ${sourceFile}")
      (new SCUnitTraverser(sourceFile, elementsWriter, relationsWriter)).traverse(unit.body)
    }
  }

  trait ScopeTracking {

    var scopeStack: List[ModelSymbol] = Nil

    val debug: Boolean
    val logTransScope: Boolean

    var depth = 0
    private def indentString: String = "  " * depth
    def scopeLog(msg: => String): Unit = {
      if(debug)
        println(s"${indentString}  $msg")
    }


    def enterScope[T](name: String, mSymbol: ModelSymbol)(fn: => T): Unit = {
      if(debug)
        println(s"${indentString}$name - ${mSymbol.csvString}")
      scopeStack = mSymbol :: scopeStack
      depth +=1
      fn
      scopeStack = scopeStack.tail
      depth -=1
    }


    def outerScope: ModelSymbol = scopeStack.tail.head
    def currentScope: ModelSymbol = scopeStack.head
    def currentGlobalScope: ModelSymbol = scopeStack.find(_.isGlobal).get

    def enterTransScope[T](name: String)(fn: => T): Unit = {

      if(debug && logTransScope) {
        println(s"${indentString}$name")
        depth +=1
        fn
        depth -=1
      } else
        fn
    }
  }


  class SCUnitTraverser(sourceFile: String, elementsWriter: ElementsWriter, relationsWriter: RelationshipsWriter) extends global.Traverser with ScopeTracking {

    import global._
    lazy val g: global.type = global
    val debug = true
    val logTransScope = true

    override def traverse(tree: Tree): Unit = {

      tree match {
        case packageDef: PackageDef =>
          val symbol = packageDef.symbol
          val mSymbol = asMSymbol(symbol)
          enterScope("PackageDef", mSymbol) {
            super.traverse(packageDef)
          }
        case treeSelect: Select =>
          enterTransScope("Select") {
            super.traverse(treeSelect)
          }
        case template: Template =>
          enterTransScope("Template")(super.traverse(template))
        case typeTree: TypeTree =>
          enterTransScope("TypeTree")(super.traverse(typeTree))
        case blockTree: Block =>
          enterTransScope("Block")(super.traverse(blockTree))
        case superTree: Super =>
          enterTransScope("Super")(super.traverse(superTree))
        case EmptyTree =>
          // do nothing
        case thisTree: This =>
          scopeLog("This")
          // do nothing
        case objectDef: ModuleDef =>
          val symbol = objectDef.symbol
          val mSymbol = asMSymbol(symbol)
          enterScope("ObjectDef", mSymbol) {
            val sSymbol = symbol.toSemantic
            val isGlobal = symbol.isSemanticdbGlobal
            elementsWriter.objectDef(isGlobal, sSymbol, sourceFile, symbol.pos.start, symbol.pos.end)

            val directSymbols = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

            if (!symbol.owner.isPackageClass) {
              val parentMSym = asMSymbol(symbol.outerClass)
              if(parentMSym != outerScope) {
                scopeLog("WARNING outerClass symbol != scope")
                scopeLog("  parent (symbol.outerClass: " + parentMSym)
                scopeLog("  outerScope:                " + outerScope)
              }
              relationsWriter.within(outerScope, mSymbol)
            }

            symbol.ancestors foreach { parentSymbol =>
              val parentMSymbol = asMSymbol(parentSymbol)
              val direct = directSymbols.contains(parentMSymbol)
              relationsWriter.extendsCls(parentMSymbol, mSymbol, direct)
            }

            super.traverse(tree)
          }

        case apply: Apply =>
          val target = asMSymbol(apply.symbol)
          val isSynthetic = target.isSynthetic
          if(!isSynthetic && !scopeStack.head.isSynthetic)
            relationsWriter.refers(currentGlobalScope, target, isSynthetic)
          super.traverse(tree)

        case classDef: ClassDef =>
          val symbol = classDef.symbol
          val isTrait = symbol.isTrait
          val mSymbol = asMSymbol(symbol)

          enterScope("ClassDef", mSymbol) {

            if (!symbol.isSynthetic) {
              if (isTrait)
                elementsWriter.traitDef(mSymbol)
              else
                elementsWriter.classDef(mSymbol)
            }

            val directSymbols = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

            symbol.ancestors foreach { parentSymbol =>
              val parentMSymbol = asMSymbol(parentSymbol)
              val direct = directSymbols.contains(parentMSymbol)
              relationsWriter.extendsCls(parentMSymbol, mSymbol, direct = direct)
              scopeLog(s"parent: ${parentSymbol.toSemantic}  $direct")
            }
            super.traverse(tree)
          }

        // *********************************************************************************************************
        case valDef: ValDef =>

          val symbol = valDef.symbol
          val mSymbol = asMSymbol(symbol)
          enterScope("ValDef-" + (if(symbol.isVar) "var" else "val"), mSymbol) {
            relationsWriter.refers(currentScope, asMSymbol(valDef.tpt.symbol), symbol.isSynthetic)
            if (symbol.isLocalToBlock) {

              def typeRels(container: ModelSymbol, typeTarget: Type): Unit = {
                val typeSymbol = typeTarget.typeSymbol
                relationsWriter.refers(currentScope, asMSymbol(typeSymbol), typeSymbol.isSynthetic)
                typeTarget.typeArgs foreach { tpe =>
                  typeRels(container, tpe)
                }
              }

              typeRels(currentScope, valDef.tpt.tpe)
            }
            if (symbol.isVar) {
              elementsWriter.varDef(mSymbol)
              scopeLog("var: " + mSymbol.csvString)
            } else {
              elementsWriter.valDef(mSymbol)
              scopeLog("val: " + mSymbol.csvString)
              val parentMSym = asMSymbol(symbol.outerClass)
              relationsWriter.within(parentMSym, mSymbol)
              relationsWriter.refers(parentMSym, mSymbol, symbol.isSynthetic)
            }

            super.traverse(tree)
          }
        case defdef: DefDef =>

          // TODO This feels wrong - this is def declType Defined field
          val declTypeDefined = defdef.isTyped
          val symbol = defdef.symbol
          val mSymbol = asMSymbol(symbol)

          enterScope("DefDef", mSymbol) {
            if (!symbol.isSynthetic && !symbol.isAccessor) {
              elementsWriter.method(mSymbol, symbol.nameString, declTypeDefined)

              val parentMSym = asMSymbol(symbol.outerClass)
              relationsWriter.within(parentMSym, mSymbol)

              val directParentSymbols = symbol.info.parents.map(t => asMSymbol(t.typeSymbol)).toSet

              symbol.overrides.foreach { overridden =>
                val overriddenOwnerMSym = asMSymbol(overridden.owner)
                val direct = directParentSymbols.contains(overriddenOwnerMSym)

                relationsWriter.overrides(mSymbol, asMSymbol(overridden), direct)
              }

              super.traverse(tree)
            }
          }

        case unknown =>
          scopeLog("--  unhandled tree" + tree.getClass)
          super.traverse(tree)
      }
    }
  }
}
