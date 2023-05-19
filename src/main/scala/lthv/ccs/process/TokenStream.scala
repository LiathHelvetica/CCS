package lthv.ccs.process

import lthv.ccs.token.MaterialisedToken

class TokenStream(tokens: Seq[MaterialisedToken]) {

  var stream = tokens

  def next(): Option[MaterialisedToken] = {
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
}
