package x1.scalaclean.test.rules.privatiser.MakePublic {

  object AppPublic extends App {
    import x2.scalaclean.test.rules.privatiser.MakePublic.Other._
    println(myVal + myVar + myDef + myObj.toString + (new myClass + (new myTrait{}).toString))
    import x2.scalaclean.test.rules.privatiser.MakePublic.Other3.{myVal => myValOn3}
    println(myValOn3)
    x2.scalaclean.test.rules.privatiser.MakePublic.Access.access2()
    x2.scalaclean.test.rules.privatiser.MakePublic.Access.access3()
  }

}
package x2.scalaclean.test.rules.privatiser.MakePublic {

  object Other {
    val myVal = 1
    var myVar = 1
    def myDef = 1
    object myObj
    trait myTrait
    class myClass
  }
  private[MakePublic] object Other2 {
    //inner refs are not marked private[MakePublic] as enclosing is
    val myVal = 1
    var myVar = 1
    def myDef = 1
    object myObj
    trait myTrait
    class myClass
  }
  object Other3 {
    //inner refs are marked private[MakePublic] as narrower than enclosing
    val myVal = 1
    private[MakePublic] var myVar = 1
    private[MakePublic] def myDef = 1
    private[MakePublic] object myObj
    private[MakePublic] trait myTrait
    private[MakePublic] class myClass
  }

  object Access{
    def access2() = {
      import x2.scalaclean.test.rules.privatiser.MakePublic.Other2._
      println(myVal + myVar + myDef + myObj.toString + (new myClass + (new myTrait {}).toString))
    }
    def access3() = {
      import x2.scalaclean.test.rules.privatiser.MakePublic.Other2._
      println(myVal + myVar + myDef + myObj.toString + (new myClass + (new myTrait {}).toString))
    }
  }


}
