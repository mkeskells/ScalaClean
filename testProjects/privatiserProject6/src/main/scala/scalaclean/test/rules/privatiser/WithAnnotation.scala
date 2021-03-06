package x1.scalaclean.test.rules.privatiser.withannotation

object Entry extends App {
  (new Entry).foo
  (new OtherClass).foo
  (new ThirdClass).bar
}

//should be private class as only accessed from companion
@deprecated
class Entry {

  //should not be private as enclosing is
  @deprecated
  def foo: Int = foo + 1

}

//should be private[withannotation] class as only accessed from package
@deprecated
class OtherClass {

  //should not be private as enclosing is same access
  @deprecated
  def foo: Int = foo + 1

}

//should be private[withannotation] class as only accessed from package
@deprecated
class ThirdClass {

  //should not be private as enclosing is same access
  @deprecated
  def bar: Int = foo + 1

  //should not be private/private[this] as tighter than enclosing
  @deprecated
  def foo: Int = foo + 1

}
