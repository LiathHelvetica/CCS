package lthv.ccs.exception

import lthv.ccs.parser.TokenStream

case class NonEmptiedStreamException(msg: String) extends Exception(msg)

object NonEmptiedStreamException {
  def apply(tokenStream: TokenStream): NonEmptiedStreamException = new NonEmptiedStreamException(
    s"${tokenStream.lineTag} Token stream was not emptied\n" +
      s"Tokens left in stream: ${tokenStream.streamToString}\n" +
      s"Initial tokens: ${tokenStream.tokensToString}"
  )
}
