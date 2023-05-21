package lthv.ccs

import lthv.ccs.preprocess.{GrammarConstructor, GrammarUtils, Preprocessor}
import lthv.ccs.parser.{Parser, Production, StyleGrouper}
import lthv.ccs.tokeniser.Tokeniser
import lthv.ccs.tokeniser.token.MaterialisedToken

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Validator {

  val ENCODING = "US-ASCII"
  val SELECTOR_GRAMMAR_PATH = "src/main/resources/grammars/selector_grammar"
  val PROPERTY_GRAMMAR_PATH = "src/main/resources/grammars/property_grammar"
  val LAST_PROPERTY_GRAMMAR_PATH = "src/main/resources/grammars/last_property_grammar"

  def validate(path: String): Seq[Throwable] = {
    val resourceTry = Try { path }
      .map(path => Source.fromFile(path, ENCODING))

    val preprocessedLines = resourceTry.map(source => source.getLines().toSeq)
      .map(lines => lines.zipWithIndex)
      .map(Preprocessor.reduceRedundantLines)

    val tokensLines = preprocessedLines match {
      case Success(lines) => {
        val (excs, tokens) = lines.map {
          case (line, i) => Tokeniser.toTokens(line, i)
        }.foldLeft((Seq.empty[Throwable], Seq.empty[Seq[MaterialisedToken]])) {
          case ((excs, tokens), Success(tokensLine)) => excs -> (tokens :+ tokensLine)
          case ((excs, tokens), Failure(exc)) => (excs :+ exc) -> tokens
        }

        if (excs.isEmpty) Right(tokens) else Left(excs)
      }
      case Failure(t) => Left(Seq(t))
    }

    val grouped = tokensLines.flatMap(lines => StyleGrouper.groupStyles(lines))

    val validationOutcome = for {
      styleTokens <- grouped
      selectorGrammar <- yieldGrammar(SELECTOR_GRAMMAR_PATH)
      propertyGrammar <- yieldGrammar(PROPERTY_GRAMMAR_PATH)
      lastPropertyGrammar <- yieldGrammar(LAST_PROPERTY_GRAMMAR_PATH)
      selectorInit <- GrammarUtils.getInitialState(selectorGrammar)
      propertyInit <- GrammarUtils.getInitialState(propertyGrammar)
      lastPropertyInit <- GrammarUtils.getInitialState(lastPropertyGrammar)
    } yield {
      Parser.parse(styleTokens, selectorGrammar, propertyGrammar, lastPropertyGrammar, selectorInit, propertyInit, lastPropertyInit)
    }

    val out = validationOutcome match {
      case Left(excs) => excs
      case Right(excs) => excs
    }

    resourceTry.foreach(_.close)

    out
  }

  def yieldGrammar(path: String): Either[Seq[Throwable], Map[String, Production]] = {
    GrammarConstructor.constructGrammar(path).toEither.fold(t => Left(Seq(t)), v => Right(v))
  }
}
