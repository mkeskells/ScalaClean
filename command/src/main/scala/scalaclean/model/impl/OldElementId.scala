package scalaclean.model.impl

import java.util.concurrent.ConcurrentHashMap

import scalafix.v1.{SemanticDocument, Symbol}

import scala.meta.Tree

case class OldElementId private(isGlobal: Boolean, symbol: Symbol) {


  def value: String = symbol.value

  def debugValue: String = s"${if (isGlobal) "G" else "L"}:$value"

  def isRootPackage: Boolean = symbol.isRootPackage

  def displayName = symbol.displayName

  def asNonEmpty: Option[OldElementId] = symbol.asNonEmpty.map(s => OldElementId(s))

  @deprecated
  def owner: OldElementId = OldElementId(symbol.owner)

  def isNone: Boolean = symbol.isNone

}

object OldElementId {

  private val cache = new ConcurrentHashMap[String, OldElementId]()

  def apply(s: Symbol): OldElementId = {
    val strRep = s.value
    if (strRep.startsWith("G:"))
      throw new IllegalArgumentException("Boom")
    apply(strRep)
  }

  def apply(s: String): OldElementId = {

    if (s.startsWith("G:") || s.startsWith("L:")) {
      cache.computeIfAbsent(s, s => {
        val isGlobal = s.startsWith("G:")
        val symbol = Symbol(s.drop(2))
        OldElementId(isGlobal, symbol)
      })
    } else {
      val symbol = Symbol(s)
      if (symbol.isGlobal)
        apply("G:" + s)
      else
        apply("L:" + s)
    }
  }


  def fromTree(tree: Tree)(implicit doc: SemanticDocument): OldElementId = {
    import scalafix.v1.{Patch => _, _}
    OldElementId(tree.symbol)
  }

  val None: OldElementId = OldElementId(Symbol.None)

  val RootPackage: OldElementId = OldElementId(Symbol.RootPackage)
}
