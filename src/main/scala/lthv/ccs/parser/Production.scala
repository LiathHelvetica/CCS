package lthv.ccs.parser

import lthv.ccs.exception.{CommittedGrammarRuleUnfulfillmentException, NonExistentRuleException}
import lthv.ccs.tokeniser.token.Token
import lthv.ccs.tokeniser.token.Token.NOTHING

trait Production {
  def name: String
  def accept(stream: TokenStream, grammar: Map[String, Production]): Boolean
}

object Production {

  case object NothingProduction extends Production {
    def name: String = NOTHING.name
    override def accept(stream: TokenStream, grammar: Map[String, Production]): Boolean = true
  }

  case class TerminalProduction(t: Token) extends Production {

    def name: String = t.name

    override def accept(stream: TokenStream, grammar: Map[String, Production]): Boolean = {
      stream.peak().exists(mt => {
        if (mt.tokenType == t) {
          stream.consume() // side effect
          true
        } else false
      })
    }
  }

  class NonTerminalProduction(nameC: String, definitionC: List[List[String]]) extends Production {

    def name: String = nameC

    val definitions: Seq[Seq[String]] = definitionC

    override def accept(stream: TokenStream, grammar: Map[String, Production]): Boolean = {

      for {
        definition <- definitions
      } yield {
        if (verifyDefinition(stream, grammar, definition)) return true
      }

      false
    }

    private def verifyDefinition(stream: TokenStream, grammar: Map[String, Production], definition: Seq[String]): Boolean = {

      definition match {
        case h :: t => if (grammar.get(h).exists(p => p.accept(stream, grammar))) { // if definition accepts
            commitToDefinition(stream, grammar, definition, t)
          } else false
        case _ => throw NonExistentRuleException(nameC, definition, stream)
      }
    }

    private def commitToDefinition(stream: TokenStream, grammar: Map[String, Production], definition: Seq[String], definitionTail: Seq[String]): Boolean = {

      definitionTail.foreach(furtherProd => {
        grammar.get(furtherProd).map(prod => {
          if (!prod.accept(stream, grammar)) {
            throw CommittedGrammarRuleUnfulfillmentException(nameC, definition, stream, furtherProd)
          }
        }).getOrElse(throw NonExistentRuleException(nameC, definition, furtherProd, stream))
      })

      true
    }
  }
}