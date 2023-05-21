package lthv.ccs.tokeniser

import lthv.ccs.tokeniser.token.Token.{ALPHA, COLON, COMMA, DOT, EXC, EXTRA_CHARS, GT, HASH, HYPHEN, IMPORTANT_KWORD, NOTHING, NUMERIC, PLUS, SEMICOLON, SPACE, STAR, TAB, UNDERSCORE, WHITESPACES}
import lthv.ccs.tokeniser.token.{FileState, MaterialisedToken, Token}

trait State {

  val id: String

  def materialiseToken(in: String): Token

  def materialiseToken(in: String, fileState: FileState): MaterialisedToken =
    MaterialisedToken(materialiseToken(in), fileState)

  def nextState(c: Char): Option[State]

  def showState(in: String): String = s"$in in $id"
}

object State {
  case object START extends State {

    override val id: String = "START"

    override def materialiseToken(in: String): Token = NOTHING

    override def nextState(c: Char): Option[State] = c.toLower match {
      case '!' | '#' | '*' | ',' | '-' | ':' | ';' | '>' | '_' => Some(DEFINITELY_OPERATOR)
      case ' ' | '\t' => Some(SINGLE_DEFINED_WHITESPACE)
      case c if c.isWhitespace => Some(WHITESPACES_STATE)
      case '(' | ')' | '/' | '\\' | '&' | '=' | '?' | '\'' | '"' | '%' => Some(DEFINITELY_EXTRA_CHARS)
      case '+' | '.' => Some(POTENTIALLY_OPERATOR)
      case c if c.isDigit => Some(NUMERIC_STATE)
      case 'i' => Some(I)
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object DEFINITELY_OPERATOR extends State {
    override val id: String = "DEFINITELY_OPERATOR"

    override def materialiseToken(in: String): Token = in match {
      case "!" => EXC
      case "#" => HASH
      case "*" => STAR
      case "," => COMMA
      case "-" => HYPHEN //misleading but that's correct
      case "_" => UNDERSCORE
      case ":" => COLON
      case ";" => SEMICOLON
      case ">" => GT
      case _ => sys.error(showState(in))
    }

    override def nextState(c: Char): Option[State] = None
  }

  case object SINGLE_DEFINED_WHITESPACE extends State {
    override val id: String = "SINGLE_DEFINED_WHITESPACE"

    override def materialiseToken(in: String): Token = in match {
      case " " => SPACE
      case "\t" => TAB
      case _ => WHITESPACES
    }

    override def nextState(c: Char): Option[State] = c match {
      case c if c.isWhitespace => Some(WHITESPACES_STATE)
      case _ => None
    }
  }

  case object WHITESPACES_STATE extends State {
    override val id: String = "WHITESPACES"

    override def materialiseToken(in: String): Token = WHITESPACES

    override def nextState(c: Char): Option[State] = c match {
      case c if c.isWhitespace => Some(WHITESPACES_STATE)
      case _ => None
    }
  }

  case object DEFINITELY_EXTRA_CHARS extends State {
    override val id: String = "DEFINITELY_EXTRA_CHARS"

    override def materialiseToken(in: String): Token = EXTRA_CHARS

    override def nextState(c: Char): Option[State] = c match {
      case '(' | ')' | '_' | '/' | '\\' | '&' | '=' | '?' | '\'' | '"' | '%' | '+' | '.' => Some(DEFINITELY_EXTRA_CHARS)
      case _ => None
    }
  }

  case object POTENTIALLY_OPERATOR extends State {
    override val id: String = "POTENTIALLY_OPERATOR"

    override def materialiseToken(in: String): Token = in match {
      case "+" => PLUS
      case "." => DOT
      case _ => EXTRA_CHARS
    }

    override def nextState(c: Char): Option[State] = c match {
      case '(' | ')' | '_' | '/' | '\\' | '&' | '=' | '?' | '\'' | '"' | '%' | '+' | '.' => Some(DEFINITELY_EXTRA_CHARS)
      case _ => None
    }
  }

  case object NUMERIC_STATE extends State {
    override val id: String = "NUMERIC_STATE"

    override def materialiseToken(in: String): Token = NUMERIC

    override def nextState(c: Char): Option[State] = c match {
      case c if c.isDigit => Some(NUMERIC_STATE)
      case _ => None
    }
  }

  case object ALPHA_STATE extends State {
    override val id: String = "ALPHA_STATE"

    override def materialiseToken(in: String): Token = ALPHA

    override def nextState(c: Char): Option[State] = c.toLower match {
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object I extends State {
    override val id: String = "i(mportant)"

    override def materialiseToken(in: String): Token = ALPHA

    override def nextState(c: Char): Option[State] = c.toLower match {
      case 'm' => Some(IM)
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object IM extends State {
    override val id: String = "im(portant)"

    override def materialiseToken(in: String): Token = ALPHA

    override def nextState(c: Char): Option[State] = c.toLower match {
      case 'p' => Some(IMP)
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object IMP extends State {
    override val id: String = "imp(ortant)"

    override def materialiseToken(in: String): Token = ALPHA

    override def nextState(c: Char): Option[State] = c.toLower match {
      case 'o' => Some(IMPO)
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object IMPO extends State {
    override val id: String = "impo(rtant)"

    override def materialiseToken(in: String): Token = ALPHA

    override def nextState(c: Char): Option[State] = c.toLower match {
      case 'r' => Some(IMPOR)
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object IMPOR extends State {
    override val id: String = "impor(tant)"

    override def materialiseToken(in: String): Token = ALPHA

    override def nextState(c: Char): Option[State] = c.toLower match {
      case 't' => Some(IMPORT)
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object IMPORT extends State {
    override val id: String = "import(ant)"

    override def materialiseToken(in: String): Token = ALPHA

    override def nextState(c: Char): Option[State] = c.toLower match {
      case 'a' => Some(IMPORTA)
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object IMPORTA extends State {
    override val id: String = "importa(nt)"

    override def materialiseToken(in: String): Token = ALPHA

    override def nextState(c: Char): Option[State] = c.toLower match {
      case 'n' => Some(IMPORTAN)
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object IMPORTAN extends State {
    override val id: String = "importan(t)"

    override def materialiseToken(in: String): Token = ALPHA

    override def nextState(c: Char): Option[State] = c.toLower match {
      case 't' => Some(IMPORTANT)
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }

  case object IMPORTANT extends State {
    override val id: String = "important"

    override def materialiseToken(in: String): Token = IMPORTANT_KWORD

    override def nextState(c: Char): Option[State] = c match {
      case c if c.isLetter => Some(ALPHA_STATE)
      case _ => None
    }
  }
}


