package scalaclean.test.references.internalIncomingReferences

class Parent {
  def foo: Unit = ()/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Parent#foo().) - scalaclean/test/references/internalIncomingReferences/XX. */
}/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Parent#) - scalaclean/test/references/internalIncomingReferences/Child# */
class Child extends Parent{
  def bar(x:Any): Unit = ()/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Child#bar().) - scalaclean/test/references/internalIncomingReferences/XX. */
}/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Child#) - scalaclean/test/references/internalIncomingReferences/XX. */
object Special {
  def myVar1 = 1 /* MISSING*/
  def myVar1_=(i: Int) = ()/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Special.`myVar1_=`().) - scalaclean/test/references/internalIncomingReferences/XX. */

  def myVar2 = 1/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Special.myVar2().) - scalaclean/test/references/internalIncomingReferences/XX. */
  def myVar2_=(i: Int) = ()/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Special.`myVar2_=`().) - scalaclean/test/references/internalIncomingReferences/XX. */

  def apply() = 7 /* MISSING*/
  def unapply(a:Any) = Some(1,2)/* MISSING*/

  def update(i: Int, j: Int) = ()/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Special.update().) - scalaclean/test/references/internalIncomingReferences/XX. */

}/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/Special.) - scalaclean/test/references/internalIncomingReferences/XX. */

object XX {
  new Child().foo
  new Child().bar(1)

  Special.myVar1 += 1

  Special.myVar2 = 9
  println(Special.myVar2)

  Special()
  val x: Any = 1/* internalIncomingReferences(scalaclean/test/references/internalIncomingReferences/XX.x.) - scalaclean/test/references/internalIncomingReferences/XX. */
  x match {
    case i: Int => ???
    case Special(a,b) => ???
  }

  Special(1) = 7
}