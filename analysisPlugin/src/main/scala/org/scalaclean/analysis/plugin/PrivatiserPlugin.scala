package org.scalaclean.analysis.plugin
import org.scalaclean.analysis.{ExtensionData, ModelSymbol, ScalaCompilerPluginComponent, VisibilityData}

import scala.reflect.internal.Flags

object PrivatiserPlugin extends ExtensionPluginFactory {
  override def create(sc: ScalaCompilerPluginComponent, paramIgnored: String)= new PrivatiserPlugin(sc)
}
class PrivatiserPlugin(val sc: ScalaCompilerPluginComponent) extends ExtensionPlugin {
  override def extendedData(mSymbol: ModelSymbol, tree: g.Tree): List[ExtensionData] = {
    tree match {
      case d : g.MemberDefApi if d.mods.isProtected =>
        val pos = d.mods.positions(Flags.PROTECTED)
        val basePos = tree.pos.start
        List(VisibilityData(pos.start - basePos,pos.end - basePos, "protected", d.mods.privateWithin.toString))
      case d : g.MemberDefApi if d.mods.isPrivate =>
        val pos = d.mods.positions(Flags.PRIVATE)
        val basePos = tree.pos.start
        List(VisibilityData(pos.start - basePos,pos.end - basePos, "private", d.mods.privateWithin.toString))
      case _ => Nil
    }
  }
}
