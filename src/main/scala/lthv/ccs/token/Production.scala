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

  class NonTerminalProduction(definitionC: List[List[String]]) extends Production {

    val definitions: Seq[Seq[String]] = definitionC

    override def accept(stream: TokenStream, grammar: Map[String, Production]): Boolean = {

      for {
        definition <- definitions
      } yield {

        definition match {
          case h :: t => {
            if (grammar.get(h).exists(p => p.accept(stream, grammar))) { // if definition accepts
              return t.foldLeft(true)((b, str) => {
                // grammar.get(str).exists(prod => b && prod.accept(stream, grammar))
                // change for foreach?
                grammar.get(str).map(prod => {
                  if (!prod.accept(stream, grammar)) {
                    throw new Exception(
                      s"Could not fulfill grammar rule ??? -> $definition\n" +
                        s"Tokens left: ${stream.stream}\n" +
                        s"Stopped at symbol: $str"
                    )
                  } else {
                    true
                  }
                }).getOrElse(throw new Exception(s"Could not find production $str"))
              })
            }
          }
          case _ => throw new Exception("EMPTY RULE ????")
        }
      }

      // I think throw was correct - it signifies that no initial symbol could be matched
      // throw UnrecognisedTokenException(stream.peak().getOrElse(throw new Exception("Empty stream")))
      false
    }
  }
}