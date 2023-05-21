package lthv.ccs.exception

import lthv.ccs.parser.TokenStream

case class EmptyStreamException(msg: String) extends Exception(msg)

object EmptyStreamException {
  def apply(ruleName: String, tokenStream: TokenStream): EmptyStreamException = new EmptyStreamException(
    s"${tokenStream.lineTag} Rule $ruleName was unable to be fulfilled AND token stream is emptied\n" +
      s"Initial tokens: ${tokenStream.tokensToString}"
  )
}
