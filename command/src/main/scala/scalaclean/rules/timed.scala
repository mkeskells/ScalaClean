package scalaclean.rules

object timed {
  var events: List[(String, Long)] = Nil

  apply("start",())

  def apply[T](name:String, fn: => T): T = {
    val result = fn
    events ::= ((name, System.currentTimeMillis()))
    result
  }

  override def toString: String = {
    events.reverse.sliding(2).map {
      case List((event, start), (_, end)) =>
        f"$event%s-15s         ${(end - start)/1000.0} sec"
    }.mkString("Timings\n  ", "\n  ", "\n")
  }
}