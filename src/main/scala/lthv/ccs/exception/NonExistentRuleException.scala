package lthv.ccs.exception

import lthv.ccs.parser.TokenStream

case class NonExistentRuleException(msg: String) extends Exception(msg)

object NonExistentRuleException {

  def apply(ruleName: String, ruleDefinition: Seq[String], tokenStream: TokenStream): NonExistentRuleException = new NonExistentRuleException(
    s"${tokenStream.lineTag} Applied rule is empty: $ruleName -> ${ruleDefinition.mkString(" ")}\n" +
      s"Tokens left in stream: ${tokenStream.streamToString}\n" +
      s"Initial stream: ${tokenStream.tokensToString}"
  )

  def apply(ruleName: String, ruleDefinition: Seq[String], subRuleName: String, tokenStream: TokenStream): NonExistentRuleException = new NonExistentRuleException(
    s"${tokenStream.lineTag} Unable to find production for $subRuleName\n" +
      s"While applying rule $ruleName -> ${ruleDefinition.mkString(" ")}\n" +
      s"Tokens left in stream: ${tokenStream.streamToString}\n" +
      s"Initial stream: ${tokenStream.tokensToString}"
  )
}
