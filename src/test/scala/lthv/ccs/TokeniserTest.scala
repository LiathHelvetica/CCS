package lthv.ccs

import lthv.ccs.tokeniser.Tokeniser
import lthv.ccs.tokeniser.token.{MaterialisedToken, Token}
import lthv.ccs.tokeniser.token.Token.{ALPHA, COLON, DOT, EXC, EXTRA_CHARS, GT, HASH, IMPORTANT_KWORD, PLUS, SPACE, TAB, WHITESPACES}
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

class TokeniserTest extends AnyFunSuite {

  def materialisedToTokenTypes(s: Seq[MaterialisedToken]): Seq[Token] = s.map(_.tokenType)

  def performTest(line: String, expected: Seq[Token]): Unit = {

    val outTry = Tokeniser.toTokens(line, 0)

    outTry match {
      case Failure(t) => fail(t)
      case Success(tokens) => assert(materialisedToTokenTypes(tokens) == expected)
    }
  }

  def performFailingTest(line: String): Unit = {

    val out = Tokeniser.toTokens(line, 0)

    assert(out.isFailure)
  }

  test("Alpha tokens recognised correctly") {

    val line = "QWE qwe zxc"
    val expected = Seq(ALPHA, SPACE, ALPHA, SPACE, ALPHA)

    performTest(line, expected)
  }

  test("Operators recognised correctly") {

    val line = "+ > . #"
    val expected = Seq(PLUS, SPACE, GT, SPACE, DOT, SPACE, HASH)

    performTest(line, expected)
  }

  test("Simple property recognised correctly") {

    val line = "\tcolor: red"
    val expected = Seq(TAB, ALPHA, COLON, SPACE, ALPHA)

    performTest(line, expected)
  }

  test("Simple selector recognised correctly") {

    val line = "qwe + qwe > qwe"
    val expected = Seq(ALPHA, SPACE, PLUS, SPACE, ALPHA, SPACE, GT, SPACE, ALPHA)

    performTest(line, expected)
  }

  test("Whitespaces recognised properly") {

    val line = " \t \t"
    val expected = Seq(WHITESPACES)

    performTest(line, expected)
  }

  test("Important recognised properly") {

    val line = "!    important"
    val expected = Seq(EXC, WHITESPACES, IMPORTANT_KWORD)

    performTest(line, expected)
  }

  test("Random chars recognised properly") {

    val line = "++ .. ?+?"
    val expected = Seq(EXTRA_CHARS, SPACE, EXTRA_CHARS, SPACE, EXTRA_CHARS)

    performTest(line, expected)
  }

  test("Reading illegal char at the end results in failure") {

    val line = "123 QWE ~"
    performFailingTest(line)
  }

  test("Reading illegal char at the beginning results in failure") {

    val line = "~123 QWE"
    performFailingTest(line)
  }

  test("Reading illegal char in the middle results in failure") {

    val line = "123 ~QWE "
    performFailingTest(line)
  }

  test("Important spelled in mixed case recognised properly") {

    val line = "!ImPoRtAnT"
    val expected = Seq(EXC, IMPORTANT_KWORD)

    performTest(line, expected)  }
}
