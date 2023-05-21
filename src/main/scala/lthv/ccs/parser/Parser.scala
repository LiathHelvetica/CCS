package lthv.ccs.parser

import lthv.ccs.exception.{EmptyStreamException, ImproperStyleDefinitionException, NonEmptiedStreamException, UnrecognisedTokenException}
import lthv.ccs.tokeniser.token.MaterialisedToken

import scala.util.{Failure, Success, Try}

object Parser {

  def parse(
    styleTokens: Seq[StyleTokens],
    selectorGrammar: Map[String, Production],
    propertyGrammar: Map[String, Production],
    lastPropertyGrammar: Map[String, Production],
    selectorInit: Production,
    propertyInit: Production,
    lastPropertyInit: Production
  ): Seq[Throwable] = {

    styleTokens.foldLeft(Seq.empty[Throwable])((excsAcc, style) => {
      excsAcc ++ validate(
        selectorInit, new TokenStream(style.selector.flatten), selectorGrammar
      ) ++ validatePropertyTokens(
        style, style.properties, propertyGrammar, propertyInit, lastPropertyGrammar, lastPropertyInit
      )
    })
  }

  def validate(initProduction: Production, tokens: TokenStream, grammar: Map[String, Production]): Seq[Throwable] = {

    Try {
      initProduction.accept(tokens, grammar)
    } match {
      case Failure(t) => Seq(t)
      case _ if tokens.stream.nonEmpty => Seq(NonEmptiedStreamException(tokens))
      case Success(true) => Seq.empty
      case Success(false) => tokens.peak() match {
        case Some(token) => Seq(UnrecognisedTokenException(token, initProduction.name, tokens))
        case _ => throw EmptyStreamException(initProduction.name, tokens)
      }
    }
  }

  def validatePropertyTokens(
    style: StyleTokens,
    propertyTokens: Seq[Seq[MaterialisedToken]],
    propertyGrammar: Map[String, Production],
    propertyInit: Production,
    lastPropertyGrammar: Map[String, Production],
    lastPropertyInit: Production
  ): Seq[Throwable] = {
    propertyTokens match {
      case h :: t if t.isEmpty => validate(lastPropertyInit, new TokenStream(h), lastPropertyGrammar)
      case h :: t => validate(propertyInit, new TokenStream(h), propertyGrammar) ++ validatePropertyTokens(
        style,
        t,
        propertyGrammar,
        propertyInit,
        lastPropertyGrammar,
        lastPropertyInit
      )
      case _ => Seq(ImproperStyleDefinitionException(style))
    }
  }
}
