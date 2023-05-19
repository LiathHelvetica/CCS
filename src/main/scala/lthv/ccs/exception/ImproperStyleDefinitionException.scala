package lthv.ccs.exception

import lthv.ccs.process.StyleTokens

case class ImproperStyleDefinitionException(msg: String) extends Exception(msg)

object ImproperStyleDefinitionException {

  def apply(lastToken: StyleTokens): ImproperStyleDefinitionException = new ImproperStyleDefinitionException(
    s"Encountered improper style $lastToken"
  )
}