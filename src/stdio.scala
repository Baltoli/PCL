class stdio {

  def send(): Either[String, Long] = {
    Left("channel")
  }

  def receive(data: Either[String, Long]) = {
    data match {
      case Left(a) => println(a)
      case Right(b) => println(b)
    }
  }

}
