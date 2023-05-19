package lthv.ccs.exception

import lthv.ccs.token.MaterialisedToken

case class UnrecognisedTokenException(msg: String) extends Exception(msg)

object UnrecognisedTokenException {

  def apply(mt: MaterialisedToken): UnrecognisedTokenException = new UnrecognisedTokenException(
    s"(${mt.fileState.line} : ${mt.fileState.iCharBeg} : ${mt.fileState.iCharEnd}) Couldn't parse token ${mt.tokenType.name} of value ${mt.fileState.value}"
  )
}
