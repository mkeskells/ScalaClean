import scala.collection.immutable

//more a test that the names are unique and refer appropriately
package name1 {
  object A extends App {
    x.immutableSeq(Array(getClass))
  }
  object x {
    def immutableSeq(arr: Array[Class[_]]): immutable.Seq[Class[_]] = immutableSeq[Class[_]](arr)

    def immutableSeq[T](arr: Array[T]): immutable.Seq[T] = if ((arr ne null) && arr.length > 0) Vector(arr: _*) else Nil
  }
}
package name2 {
  object A extends App {
    x.immutableSeq(Array(1,2,3))
  }
  object x {
    def immutableSeq(arr: Array[Class[_]]): immutable.Seq[Class[_]] = immutableSeq[Class[_]](arr)

    def immutableSeq[T](arr: Array[T]): immutable.Seq[T] = if ((arr ne null) && arr.length > 0) Vector(arr: _*) else Nil
  }
}
