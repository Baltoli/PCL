package uk.ac.cam.bsc28.diss.VM

/**
  * Similarly to a channel, provides a simple type-safe abstraction
  * over accessing variables.
  *
  * @param n The variable name.
  */
case class Variable(n: String) {
  val name = n
}
