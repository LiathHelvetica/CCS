package lthv.ccs

import lthv.ccs.preprocess.Preprocessor
import org.scalatest.funsuite.AnyFunSuite

class PreprocessorTest extends AnyFunSuite {

  test("Comments are removed properly") {

    val code = "this is code"
    val in = Seq(s"$code\\\\ this is comment")

    val out = Preprocessor.removeComments(in)

    assert(out == Seq(code))
  }
}
