package lthv.ccs.exception

import lthv.ccs.parser.TokenStream
import lthv.ccs.tokeniser.token.MaterialisedToken

case class UnrecognisedTokenException(msg: String) extends Exception(msg)

object UnrecognisedTokenException {

  def apply(mt: MaterialisedToken, ruleName: String, tokenStream: TokenStream): UnrecognisedTokenException = new UnrecognisedTokenException(
    s"(${mt.fileState.line} : ${mt.fileState.iCharBeg} : ${mt.fileState.iCharEnd}) Couldn't parse token ${mt.tokenType.name} of value ${mt.fileState.value}\n" +
      s"While applying rule: $ruleName\n" +
      s"Tokens left: ${tokenStream.streamToString}\n" +
      s"Initial tokens: ${tokenStream.tokensToString}"
  )
}
