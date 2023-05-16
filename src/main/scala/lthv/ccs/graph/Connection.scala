package lthv.ccs.graph

trait Connection {

  val to: Node

  def isCharAcceptable(c: Char): Boolean
}
