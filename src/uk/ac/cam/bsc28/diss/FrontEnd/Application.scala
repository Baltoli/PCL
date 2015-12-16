package uk.ac.cam.bsc28.diss.FrontEnd

object Application extends App {
  val program =
    """
      |in @stdio(x: Int).
      |out @stdio(x).
      |end
    """.stripMargin
  print(program)
}
