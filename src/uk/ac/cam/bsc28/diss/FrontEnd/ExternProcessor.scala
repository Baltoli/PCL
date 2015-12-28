package uk.ac.cam.bsc28.diss.FrontEnd

object ExternProcessor {

  case class ExternChannel(c: String)
  type ProcessedSource = (List[ExternChannel], String)

  private val externRegex = """(?s)^external @((?:[a-z]|_)+)(.*)$""".r

  private def preprocessSource(prog: String, acc: List[ExternChannel]): ProcessedSource = {
    prog trim match {
      case externRegex(name, rest) => preprocessSource(rest, ExternChannel(name) :: acc)
      case _ => (acc reverse, prog)
    }
  }

  def preprocessSource(prog: String): ProcessedSource = {
    preprocessSource(prog, List())
  }

}
