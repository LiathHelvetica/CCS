package lthv.ccs

import lthv.ccs.preprocess.Preprocessor

import scala.io.Source
import scala.util.Try

object Main {

  val ENCODING = "US-ASCII"

  def main(args: Array[String]): Unit = {
    val t = Try { args(0) }
      .map(path => Source.fromFile(path, ENCODING))
      .map(source => source.getLines().toSeq) // unsafe, use state monad (?)
      .map(Preprocessor.removeComments)
  }
}