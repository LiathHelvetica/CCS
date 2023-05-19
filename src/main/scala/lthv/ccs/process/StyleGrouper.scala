package lthv.ccs.process

import lthv.ccs.exception.{ImproperStyleDefinitionException, IndentationException}
import lthv.ccs.token.MaterialisedToken
import lthv.ccs.token.Token.{SPACE, TAB, WHITESPACES}

object StyleGrouper {

  val TOKEN = 1

  def groupStyles(lines: Seq[Seq[MaterialisedToken]]): Either[Seq[Throwable], Seq[StyleTokens]] = {
    val (result, lastToken, _, _) = lines.foldLeft((
      Seq.empty[Either[Throwable, StyleTokens]],
      StyleTokens(),
      Seq.empty[Throwable],
      Seq.empty[Int]
    )) {
      case ((acc, style, excs, stack), tokenLine) => {
        (stack, tokenLine) match {
          case (stack, token :: tokenTail) if continuedTab(stack, token) => encounterContinuedTab(acc, style, excs, stack, tokenTail)
          case (stack, token :: tokenTail) if continuedNonIndent(stack, token) => encounterContinuedNonIndent(acc, style, excs, stack, tokenLine)
          case (stack, token :: tokenTail) if pushedTab(stack, token) => encounterPushedTab(acc, style, excs, stack, tokenTail)
          case (stack, token :: tokenTail) if poppedTab(stack, token) => encounterPoppedTab(acc, style, excs, stack, tokenLine)
          case (_, tokenLine) if tokenLine.isEmpty => encounterEmptyTokenLine(acc, style, excs, stack)
          case (_, _) => encounterError(acc, style, excs, stack, tokenLine)
        }
      }
    }

    val finalResult = result :+ (if (lastToken.properties.isEmpty) {
      Left(ImproperStyleDefinitionException(lastToken))
    } else {
      Right(lastToken)
    })

    splitEither(finalResult)
  }

  def continuedTab(stack: Seq[Int], firstToken: MaterialisedToken): Boolean = {
    stack == Seq(TOKEN) && firstToken.tokenType == TAB
  }

  def continuedNonIndent(stack: Seq[Int], firstToken: MaterialisedToken): Boolean = {
    stack == Seq() && firstToken.tokenType != TAB && firstToken.tokenType != WHITESPACES && firstToken.tokenType != SPACE
  }

  def pushedTab(stack: Seq[Int], firstToken: MaterialisedToken): Boolean = {
    stack == Seq() && firstToken.tokenType == TAB
  }

  def poppedTab(stack: Seq[Int], firstToken: MaterialisedToken): Boolean = {
    stack == Seq(TOKEN) && firstToken.tokenType != TAB && firstToken.tokenType != WHITESPACES && firstToken.tokenType != SPACE
  }

  def encounterEmptyTokenLine(
    acc: Seq[Either[Throwable, StyleTokens]],
    style: StyleTokens,
    excs: Seq[Throwable],
    stack: Seq[Int]
  ): (Seq[Either[Throwable, StyleTokens]], StyleTokens, Seq[Throwable], Seq[Int]) = {
    val t = new Exception("ENCOUNTERED EMPTY LINE IN STYLE GROUPER")
    (acc :+ Left(t), style, excs :+ t, stack)
  }

  def encounterContinuedTab(
    acc: Seq[Either[Throwable, StyleTokens]],
    style: StyleTokens,
    excs: Seq[Throwable],
    stack: Seq[Int],
    tokenTail: Seq[MaterialisedToken]
  ): (Seq[Either[Throwable, StyleTokens]], StyleTokens, Seq[Throwable], Seq[Int]) = {
    (acc, style.copy(properties = style.properties :+ tokenTail), excs, stack)
  }

  def encounterContinuedNonIndent(
    acc: Seq[Either[Throwable, StyleTokens]],
    style: StyleTokens,
    excs: Seq[Throwable],
    stack: Seq[Int],
    tokens: Seq[MaterialisedToken]
  ): (Seq[Either[Throwable, StyleTokens]], StyleTokens, Seq[Throwable], Seq[Int]) = {
    (acc, style.copy(selector = style.selector :+ tokens), excs, stack)
  }

  def encounterPushedTab(
    acc: Seq[Either[Throwable, StyleTokens]],
    style: StyleTokens,
    excs: Seq[Throwable],
    stack: Seq[Int],
    tokenTail: Seq[MaterialisedToken]
  ): (Seq[Either[Throwable, StyleTokens]], StyleTokens, Seq[Throwable], Seq[Int]) = {
    (acc, style.copy(properties = style.properties :+ tokenTail), excs, stack :+ TOKEN)
  }

  def encounterPoppedTab(
    acc: Seq[Either[Throwable, StyleTokens]],
    style: StyleTokens,
    excs: Seq[Throwable],
    stack: Seq[Int],
    tokens: Seq[MaterialisedToken]
  ): (Seq[Either[Throwable, StyleTokens]], StyleTokens, Seq[Throwable], Seq[Int]) = {
    (acc :+ Right(style), StyleTokens(Seq(tokens), Seq.empty), excs, Seq.empty)
  }

  def encounterError(
    acc: Seq[Either[Throwable, StyleTokens]],
    style: StyleTokens,
    excs: Seq[Throwable],
    stack: Seq[Int],
    tokenLine: Seq[MaterialisedToken]
  ): (Seq[Either[Throwable, StyleTokens]], StyleTokens, Seq[Throwable], Seq[Int]) = {
    val t = IndentationException(tokenLine)
    (acc :+ Left(t), style, excs :+ t, stack)
  }

  def splitEither(s: Seq[Either[Throwable, StyleTokens]]): Either[Seq[Throwable], Seq[StyleTokens]] = {
    val (excs, tokens) = s.foldLeft((Seq.empty[Throwable], Seq.empty[StyleTokens])) {
      case ((excs, tokens), Left(t)) => (excs :+ t, tokens)
      case ((excs, tokens), Right(t)) => (excs, tokens :+ t)
    }

    if (excs.isEmpty) {
      Right(tokens)
    } else Left(excs)
  }
}
