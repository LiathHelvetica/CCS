package lthv.ccs.token

import lthv.ccs.exception.UnrecognisedTokenException
import lthv.ccs.process.TokenStream

trait Production {
  def accept(stream: TokenStream, grammar: Map[String, Production]): Boolean
}

object Production {

  case object NothingProduction extends Production {
    override def accept(stream: TokenStream, grammar: Map[String, Production]): Boolean = true
  }

  case class TerminalProduction(t: Token) extends Production {

    override def accept(stream: TokenStream, grammar: Map[String, Production]): Boolean = {
      stream.peak().exists(mt => {
        if (mt.tokenType == t) {
          stream.next() // side effect
          true
        } else false
      })
    }
  }

  class NonTerminalProduction(definitionC: Seq[Seq[String]]) extends Production {

    val definitions: Seq[Seq[String]] = definitionC

    override def accept(stream: TokenStream, grammar: Map[String, Production]): Boolean = {

      for {
        definition <- definitions
      } yield {

        definition match {
          case h :: t => {
            if (grammar.get(h).exists(p => p.accept(stream, grammar))) { // if definition accepts
              return t.foldLeft(true)((b, str) => {
                grammar.get(str).exists(prod => b && prod.accept(stream, grammar))
              })
            }
          }
          case _ => throw new Exception("EMPTY RULE ????")
        }
      }

      throw UnrecognisedTokenException(stream.peak().getOrElse(throw new Exception("Empty stream")))
    }
  }
}