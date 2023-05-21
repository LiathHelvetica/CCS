package lthv.ccs

import scala.util.{Try, Success, Failure}

object Main {

  def main(args: Array[String]): Unit = {

    val res = Try { args(0) }
      .map(path => path -> Validator.validate(path))

    res match {
      case Failure(t) => throw t
      case Success((path, s)) if s.isEmpty => println(s"File $path is VALID")
      case Success((path, s)) => {
        println(s"File $path is INVALID due to following reasons:\n")
        println(s"${s.map(_.getMessage).mkString("\n\n")}")
      }
    }
  }
}