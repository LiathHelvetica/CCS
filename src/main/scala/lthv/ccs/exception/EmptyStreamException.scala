package lthv.ccs.exception

import lthv.ccs.token.Token

case class EmptyStreamException(msg: String) extends Exception(msg)

object EmptyStreamException {
  def apply(t: Token): EmptyStreamException = new EmptyStreamException(s"Could not read token ${t.name} from stream")
}
