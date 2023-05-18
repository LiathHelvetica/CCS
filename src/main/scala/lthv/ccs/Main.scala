package lthv.ccs

import lthv.ccs.preprocess.Preprocessor
import lthv.ccs.process.Tokeniser
import lthv.ccs.token.MaterialisedToken

import scala.io.Source
import scala.util.{Failure, Left, Right, Success, Try}

object Main {

  val ENCODING = "US-ASCII"

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

    val tokens = tokensLines.map(_.flatten)

    resourceTry.foreach(_.close)
  }
}