package lthv.ccs.parser

import lthv.ccs.tokeniser.token.MaterialisedToken

class TokenStream(tokensC: Seq[MaterialisedToken]) {

  val tokens = tokensC
  var stream = tokensC

  def consume(): Option[MaterialisedToken] = {
    val (head, tail) = stream match {
      case h :: t => Some(h) -> t
      case _ => None -> Seq.empty
    }
    stream = tail
    head
  }

  def peak(): Option[MaterialisedToken] = {
    stream.headOption
  }

  def tokensToString: String = tokens.map(_.tokenType.name).mkString(" ")

  def streamToString: String = stream.map(_.tokenType.name).mkString(" ")

  def lineTag: String = {
    val line = stream.headOption
      .map(_.fileState.line.toString)
      .orElse(tokens.lastOption.map(_.fileState.line.toString))
      .getOrElse("?")
    val pos = stream.headOption.map(mt => {
      mt.fileState.iCharBeg.toString -> mt.fileState.iCharEnd.toString
    }).orElse(tokens.lastOption.map(mt => {
      val iLast = mt.fileState.iCharEnd.toString
      iLast -> iLast
    })).getOrElse("?" -> "?")
    s"($line : ${pos._1} : ${pos._2})"
  }
}
