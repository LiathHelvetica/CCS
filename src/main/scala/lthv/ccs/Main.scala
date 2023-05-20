package lthv.ccs

import lthv.ccs.preprocess.{GrammarConstructor, GrammarUtils, Preprocessor}
import lthv.ccs.process.{StyleGrouper, TokenStream, Tokeniser}
import lthv.ccs.token.{MaterialisedToken, Production}

import scala.io.Source
import scala.util.{Failure, Left, Right, Success, Try}

object Main {

  val ENCODING = "US-ASCII"
  val SELECTOR_GRAMMAR_PATH = "src/main/resources/selector_grammar"
  val PROPERTY_GRAMMAR_PATH = "src/main/resources/property_grammar"
  val LAST_PROPERTY_GRAMMAR_PATH = "src/main/resources/last_property_grammar"

  def main(args: Array[String]): Unit = {
    val resourceTry = Try { args(0) }
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

    val selectorGrammarEith = yieldGrammar(SELECTOR_GRAMMAR_PATH)
    val propertyGrammarEith = yieldGrammar(PROPERTY_GRAMMAR_PATH)
    val lastPropertyGrammarPath = yieldGrammar(LAST_PROPERTY_GRAMMAR_PATH)

    val validationOutcome = for {
      selectorGrammar <- selectorGrammarEith
      selectorInit <- GrammarUtils.getInitialState(selectorGrammar)
      propertyGrammar <- propertyGrammarEith
      propertyInit <- GrammarUtils.getInitialState(propertyGrammar)
      lastPropertyGrammar <- lastPropertyGrammarPath
      lastPropertyInit <- GrammarUtils.getInitialState(lastPropertyGrammar)
      styleTokens <- grouped
    } yield {
      Try {
        styleTokens.foreach(style => {
          val selectorTokens = new TokenStream(style.selector.flatten)
          val outSelector = selectorInit.accept(selectorTokens, selectorGrammar)
          val outProperty = validatePropertyTokens(style.properties, propertyGrammar, propertyInit, lastPropertyGrammar, lastPropertyInit)
          println(s"STYLE: $outSelector, $outProperty")
        })
      }
    }

    validationOutcome.fold(ts => throw ts.head, res => println(res))

    resourceTry.foreach(_.close)
  }

  def yieldGrammar(path: String): Either[Seq[Throwable], Map[String, Production]] = {
    GrammarConstructor.constructGrammar(path).toEither.fold(t => Left(Seq(t)), v => Right(v))
  }

  def validatePropertyTokens(
    propertyTokens: Seq[Seq[MaterialisedToken]],
    propertyGrammar: Map[String, Production],
    propertyInit: Production,
    lastPropertyGrammar: Map[String, Production],
    lastPropertyInit: Production
  ): Boolean = {
    propertyTokens match {
      case h :: t if t.isEmpty => lastPropertyInit.accept(new TokenStream(h), lastPropertyGrammar)
      case h :: t => propertyInit.accept(new TokenStream(h), propertyGrammar) && validatePropertyTokens(
        t,
        propertyGrammar,
        propertyInit,
        lastPropertyGrammar,
        lastPropertyInit
      )
      case _ => throw new Exception("PARSING EMPTY PROPERTIES PART")
    }
  }
}