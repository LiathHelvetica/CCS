package lthv.ccs.graph

trait Node {

  val id: String

  def isTerminal: Boolean = false
}

trait TerminalNode extends Node {
  override def isTerminal: Boolean = true
}
