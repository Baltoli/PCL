package uk.ac.cam.bsc28.diss.VM

/**
  * A channel in the language is similar to an Erlang atom or a Ruby symbol -
  * i.e. an element of a set of mutually distinct items. In the language syntax,
  * these symbols are given by colon-prefixed strings (e.g. :stdio or :some_channel).
  *
  * Channels are atomic - they have no underlying structure or information beyond what
  * they do simply by existing.
  *
  * @param n The 'underlying' name of the channel. Like x in the ruby symbol :x
  */
case class Channel(n: String) {
  val name = n
}