package lthv.ccs.exception

import lthv.ccs.token.MaterialisedToken

case class IndentationException(msg: String) extends Exception(msg)

object IndentationException {

  def apply(tokenLine: Seq[MaterialisedToken]): IndentationException = new IndentationException(
    s"Encountered improper line during grouping, most likely indentation issue: ${tokenLine.map(t => t.tokenType.name)}"
  )
}
