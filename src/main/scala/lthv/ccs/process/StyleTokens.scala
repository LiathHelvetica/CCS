package lthv.ccs.process

import lthv.ccs.token.MaterialisedToken

case class StyleTokens(
  selector: Seq[Seq[MaterialisedToken]] = Seq.empty,
  properties: Seq[Seq[MaterialisedToken]] = Seq.empty
)
