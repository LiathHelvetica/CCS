package lthv.ccs.preprocess

import scala.annotation.tailrec

object Preprocessor {

  def reduceRedundantLines(contents: Seq[(String, Int)]): Seq[(String, Int)] = {
    contents.foldLeft(Seq.empty[(String, Int)])((acc, line) => {

      val uncommentedLine = line._1.split("\\\\")(0)

      val trimmedLine = trimRight(uncommentedLine)

      trimmedLine match {
        case l if l.isEmpty => acc // empty lines and whitespaced comments - removed
        case l => acc :+ (l -> line._2)
      }
    })
  }

  @tailrec
  private def trimRight(acc: String): String = {
    acc match {
      case "" => ""
      case s if s.charAt(s.length - 1).isWhitespace => trimRight(s.substring(0, s.length - 1))
      case s => s
    }
  }
}

