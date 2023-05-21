package lthv.ccs.parser

import lthv.ccs.tokeniser.token.MaterialisedToken

case class StyleTokens(
  selector: Seq[Seq[MaterialisedToken]] = Seq.empty,
  properties: Seq[Seq[MaterialisedToken]] = Seq.empty
)
