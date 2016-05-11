package uk.ac.cam.bsc28.diss.CodeGeneration

object LabelGenerator {

  private var next = "a"

  def nextLabel(): String = {
    this synchronized {
      val ret = next
      next = lexicalSuccessor(next)
      ret
    }
  }

  private def lexicalSuccessor(s: String) = {
    val front = s.substring(0, s.length - 1)
    val end = s.last
    val suffix = if (end == 'z') {
      "za"
    } else {
      (end + 1).toChar.toString
    }
    front + suffix
  }

}
