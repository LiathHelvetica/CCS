package lthv.ccs.preprocess

import lthv.ccs.token.Production

object GrammarUtils {

  val SPLITTER = "_>"
  val INITIAL_CONSTRUCTION = "START"

  def getInitialState(grammar: Map[String, Production]): Either[Seq[Throwable], Production]= {
    grammar.get(INITIAL_CONSTRUCTION) match {
      case Some(p) => Right(p)
      case _ => Left(Seq(new Exception("GRAMMAR DOES NOT CONTAIN INITIAL SYMBOL")))
    }
  }
}
