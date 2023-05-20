package lthv.ccs.preprocess

import lthv.ccs.preprocess.GrammarUtils.SPLITTER
import lthv.ccs.token.Production
import lthv.ccs.token.Production.{NonTerminalProduction, NothingProduction, TerminalProduction}
import lthv.ccs.token.Token.{NOTHING, TOKEN_MAP}

import scala.io.Source
import scala.util.{Success, Try}

object GrammarConstructor {

  val ENCODING = "US-ASCII"

  def constructGrammar(fileName: String): Try[Map[String, Production]] = {
    val sourceTry = Success(fileName)
      .map(path => Source.fromFile(path, ENCODING))

    val out = sourceTry.map(source => source.getLines().toSeq.filterNot(_.isBlank))
      .map(_.map(line => {
        val splitted = line.split(SPLITTER)
        splitted(0).trim -> splitted(1).trim
      }))
      .map(_.groupMap(t => t._1)(t => t._2.split("\\s+").toSeq))
      .map(grammar => {
        grammar.map {
          case (k, v) => k -> new NonTerminalProduction(v.map(_.toList).toList)
        } ++ TOKEN_MAP.map {
          case (k, v) => k -> TerminalProduction(v)
        } ++ Map(NOTHING.name -> NothingProduction)
      })

    sourceTry.foreach(s => s.close())

    out
  }
}
