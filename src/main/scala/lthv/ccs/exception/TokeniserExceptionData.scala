package lthv.ccs.exception

import lthv.ccs.tokeniser.TokeniserState

case class TokeniserExceptionData(invalidChar: Char, line: Int, iChar: Int)

object TokeniserExceptionData {
  def apply(c: Char, tokeniserState: TokeniserState): TokeniserExceptionData = TokeniserExceptionData(
    c,
    tokeniserState.fileState.line,
    tokeniserState.fileState.iCharEnd
  )
}
