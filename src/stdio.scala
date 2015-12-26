class stdio {

  def send(): Either[String, Long] = {
    val text = readLine("> ")
    try {
      val intValue = text.toLong
      Right(intValue)
    } catch {
      case e: NumberFormatException => Left(text)
    }
  }

  def receive(data: Either[String, Long]) = {
    data match {
      case Left(a) => println(a)
      case Right(b) => println(b)
    }
  }

}
