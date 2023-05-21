package lthv.ccs.exception

import lthv.ccs.tokeniser.token.MaterialisedToken

case class IndentationException(msg: String) extends Exception(msg)

object IndentationException {

  def apply(tokenLine: Seq[MaterialisedToken]): IndentationException = new IndentationException({

    val tokens = tokenLine.map(_.tokenType.name).mkString(" ")

    tokenLine.headOption.map(mt => mt.fileState.line) match {
      case Some(i) => s"Line $i deemed improper, most likely indentation issue\nToken line: $tokens"
      case _ => s"Encountered improper line during grouping, most likely indentation issue\nToken line: $tokens"
    }
  })
}
