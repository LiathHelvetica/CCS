package lthv.ccs.exception

import lthv.ccs.parser.TokenStream

case class CommittedGrammarRuleUnfulfillmentException(msg: String) extends Exception(msg)

object CommittedGrammarRuleUnfulfillmentException {

  def apply(ruleName: String, ruleDefinition: Seq[String], tokenStream: TokenStream, finalSymbol: String): CommittedGrammarRuleUnfulfillmentException = {
    new CommittedGrammarRuleUnfulfillmentException(
      s"${tokenStream.lineTag} Could not fulfill grammar rule $ruleName -> ${ruleDefinition.mkString(" ")}\n" +
        s"Stopped at symbol: $finalSymbol\n" +
        s"Tokens left in stream: ${tokenStream.streamToString}\n" +
        s"Initial tokens: ${tokenStream.tokensToString}"
    )
  }
}
