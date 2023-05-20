package lthv.ccs

import lthv.ccs.preprocess.Preprocessor
import org.scalatest.funsuite.AnyFunSuite

class PreprocessorTest extends AnyFunSuite {

  test("Comments are removed properly") {

    val code = "this is code"
    val in = Seq(s"$code// this is comment" -> 0)

    val out = Preprocessor.reduceRedundantLines(in)

    assert(out == Seq(code -> 0))
  }

  test("Lines are right-trimmed properly") {

    val code = " this is code"
    val in = Seq(s"$code          // this is comment" -> 0)

    val out = Preprocessor.reduceRedundantLines(in)

    assert(out == Seq(code -> 0))
  }
}
