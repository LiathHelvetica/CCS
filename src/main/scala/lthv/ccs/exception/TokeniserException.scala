package lthv.ccs.exception

case class TokeniserException(msg: String) extends Exception(msg)

object TokeniserException {

  def apply(data: Seq[TokeniserExceptionData]): TokeniserException = new TokeniserException(
    data.foldLeft(s"Invalid characters in line ${data.headOption.map(_.line).getOrElse(-1)}:")((msg, d) =>
      msg ++ s" ${d.iChar} -> ${d.invalidChar},"
    )
  )
}
