package scalaclean.scratch

object ScratchMain {
  def main(args: Array[String]): Unit = {
    val p : Parent = new Child()
    println(p.baz)
  }
}

abstract class Parent {
  def baz : List[String]
}

class Child extends Parent {
  override lazy val baz : List[String] = Nil
}
