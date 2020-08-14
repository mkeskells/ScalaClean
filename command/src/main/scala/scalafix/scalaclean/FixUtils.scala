package scalafix.scalaclean

import scalafix.internal.v1.InternalSemanticDoc
import scalafix.v1.SemanticDocument.Error
import scalafix.v1.{SemanticDocument, SyntacticDocument}

import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.symtab.SymbolTable
import scala.meta.io.RelativePath
@deprecated
object FixUtils {
@deprecated
  private[scalafix] def fromPath(
                                  doc: SyntacticDocument,
                                  path: RelativePath,
                                  classLoader: ClassLoader,
                                  symtab: SymbolTable
                                ): SemanticDocument = {
    import scala.collection.JavaConverters._
    val reluri = path.toNIO.iterator().asScala.mkString("/")

    val semanticdbReluri = s"META-INF/semanticdb/$reluri.semanticdb"
    Option(classLoader.getResourceAsStream(semanticdbReluri)) match {
      case Some(inputStream) =>
        val sdocs =
          try TextDocuments.parseFrom(inputStream).documents
          finally inputStream.close()
        val sdoc = sdocs.find(_.uri == reluri).getOrElse {
          println(s" xx $reluri")
          sdocs.foreach { sd =>
            println(s"  yy ${sd.uri}   ${reluri == sd.uri}")
          }
          throw Error.MissingTextDocument(reluri)
        }
        val impl = new InternalSemanticDoc(doc, sdoc, symtab)
        new SemanticDocument(impl)
      case None =>
        throw Error.MissingSemanticdb(semanticdbReluri)
    }
  }

}
