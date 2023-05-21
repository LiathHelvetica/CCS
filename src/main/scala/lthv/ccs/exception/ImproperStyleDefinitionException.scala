package lthv.ccs.exception

import lthv.ccs.parser.StyleTokens

case class ImproperStyleDefinitionException(msg: String) extends Exception(msg)

object ImproperStyleDefinitionException {

  def apply(lastToken: StyleTokens): ImproperStyleDefinitionException = new ImproperStyleDefinitionException({
    val msgOpt = for {
      firstSelectorLine <- lastToken.selector.headOption
      firstSelectorToken <- firstSelectorLine.headOption
    } yield {
      s"Style from line ${firstSelectorToken.fileState.line} has no properties"
    }
   msgOpt.getOrElse(s"Encountered improper style with no properties $lastToken")
  })
}