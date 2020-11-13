package scalaclean.test.rules.deadcode

object Entry extends App {
  (new entry01.Entry01).foo
  (new entry02.Entry02).foo
  (new entry03.Entry03).foo
  (new entry04.Entry04).foo

  (new entry11.Entry11).foo
  (new entry12.Entry12).foo
  (new entry13.Entry13).foo
  (new entry14.Entry14).foo
}

package entry01 {

  class Entry01 extends P1 with P2

  trait P1 {
    def foo: Int

    def unused: Int = 1
  }

  trait P2 {
    def foo: Int = 1

    def unused2: Int = 1
  }

}

package entry02 {

  class Entry02 extends P1 with P2

  trait P1 {
    def foo: Int = 1

    def unused: Int = 1
  }

  trait P2 {
    def foo: Int

    def unused2: Int = 1
  }

}

package entry03 {

  class Entry03 extends P1 with P2

  abstract class P1 {
    def foo: Int

    def unused: Int = 1
  }

  trait P2 {
    def foo: Int = 1

    def unused2: Int = 1
  }

}

package entry04 {

  class Entry04 extends P1 with P2

  abstract class P1 {
    def foo: Int = 1

    def unused: Int = 1
  }

  trait P2 {
    def foo: Int

    def unused2: Int = 1
  }

}

package entry11 {

  class Entry11 extends P1 with P2 {
    override def foo = 10
  }

  trait P1 {
    def foo: Int

    def unused: Int = 1
  }

  trait P2 {
    def foo: Int = 1

    def unused2: Int = 1
  }

}

package entry12 {

  class Entry12 extends P1 with P2 {
    override def foo = 10
  }

  trait P1 {
    def foo: Int = 1

    def unused: Int = 1
  }

  trait P2 {
    def foo: Int

    def unused2: Int = 1
  }

}

package entry13 {

  class Entry13 extends P1 with P2 {
    override def foo = 10
  }

  abstract class P1 {
    def foo: Int

    def unused: Int = 1
  }

  trait P2 {
    def foo: Int = 1

    def unused2: Int = 1
  }

}

package entry14 {

  class Entry14 extends P1 with P2 {
    override def foo = 10
  }

  abstract class P1 {
    def foo: Int = 1

    def unused: Int = 1
  }

  trait P2 {
    def foo: Int

    def unused2: Int = 1
  }

}
