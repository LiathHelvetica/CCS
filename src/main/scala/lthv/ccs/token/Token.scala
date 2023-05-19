package lthv.ccs.token

trait Token {
  val name: String          // ALPHA
  val definition: String    // "[a-zA-Z]"
}

object Token {
  case object NOTHING extends Token {
    override val name: String = "NOTHING"
    override val definition: String = ""
  }

  case object COLON extends Token {
    override val name: String = "COLON"
    override val definition: String = ":"
  }

  case object SEMICOLON extends Token {
    override val name: String = "SEMICOLON"
    override val definition: String = ";"
  }

  case object HASH extends Token {
    override val name: String = "HASH"
    override val definition: String = "#"
  }

  case object DOT extends Token {
    override val name: String = "DOT"
    override val definition: String = "."
  }

  case object GT extends Token {
    override val name: String = "GT"
    override val definition: String = ">"
  }

  case object PLUS extends Token {
    override val name: String = "PLUS"
    override val definition: String = "+"
  }

  case object STAR extends Token {
    override val name: String = "STAR"
    override val definition: String = "*"
  }

  case object COMMA extends Token {
    override val name: String = "COMMA"
    override val definition: String = ","
  }

  case object HYPHEN extends Token {
    override val name: String = "HYPHEN"
    override val definition: String = "-"
  }

  case object IMPORTANT_KWORD extends Token {
    override val name: String = "IMPORTANT_KWORD"
    override val definition: String = "important"
  }

  case object SPACE extends Token {
    override val name: String = "SPACE"
    override val definition: String = " "
  }

  case object TAB extends Token {
    override val name: String = "TAB"
    override val definition: String = "\t"
  }

  case object WHITESPACES extends Token {
    override val name: String = "WHITESPACES"
    override val definition: String = "[[:whitespace: /, :space:, :tab:]]*"
  }

  case object EXC extends Token {
    override val name: String = "EXC"
    override val definition: String = "!"
  }

  case object ALPHA extends Token {
    override val name: String = "ALPHA"
    override val definition: String = "[a-zA-Z]+"
  }

  case object NUMERIC extends Token {
    override val name: String = "NUMERIC"
    override val definition: String = "[0-9]+"
  }

  case object EXTRA_CHARS extends Token {
    override val name: String = "EXTRA_CHARS"
    override val definition: String = "[_\\/().\"'+?&=]+"
  }

  val TOKENS: Set[Token] = Set(COLON, SEMICOLON, HASH, DOT, GT, PLUS, STAR, COMMA, HYPHEN, IMPORTANT_KWORD, SPACE, TAB, WHITESPACES, EXC, ALPHA, NUMERIC, EXTRA_CHARS)

  val TOKEN_MAP = TOKENS.map(t => t.name -> t).toMap
}


