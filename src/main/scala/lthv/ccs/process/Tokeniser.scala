package lthv.ccs.process

import lthv.ccs.exception.{TokeniserException, TokeniserExceptionData}
import lthv.ccs.graph.State.START
import lthv.ccs.token.{FileState, MaterialisedToken}

import scala.util.{Failure, Success, Try}

object Tokeniser {

  private case class Acc(materialisedTokens: Seq[Either[TokeniserExceptionData, MaterialisedToken]], tokeniserState: TokeniserState)

  def toTokens(s: String, line: Int): Try[Seq[MaterialisedToken]] = {

    val acc = s.foldLeft(Acc(Seq.empty, TokeniserState(line)))((acc, c) => {
      acc.tokeniserState.state.nextState(c) match {
        case Some(state) => acc.copy(tokeniserState = acc.tokeniserState.nextNonMaterialisedState(state, c))
        case None => START.nextState(c) match {
          case Some(nextState) => acc.copy(
            materialisedTokens = acc.materialisedTokens :+ Right(acc.tokeniserState.materialiseToken),
            tokeniserState = TokeniserState(acc.tokeniserState, c, nextState)
          )
          case None => acc.copy(
            materialisedTokens = acc.materialisedTokens :+ Left(TokeniserExceptionData(c, acc.tokeniserState)),
            tokeniserState = TokeniserState.getStateAfterFailure(acc.tokeniserState)
          )
        }
      }
    })

    val (exceptionDatas, tokens) = acc.copy(materialisedTokens = acc.materialisedTokens :+ Right(acc.tokeniserState.materialiseToken))
      .materialisedTokens.foldLeft((Seq.empty[TokeniserExceptionData], Seq.empty[MaterialisedToken])) {
      case ((exceptionDatas, tokens), eith) => eith match {
        case Left(exceptionData) => (exceptionDatas :+ exceptionData) -> tokens
        case Right(token) => exceptionDatas -> (tokens :+ token)
      }
    }

    if (exceptionDatas.isEmpty) {
      Success(tokens)
    } else {
      Failure(TokeniserException(exceptionDatas))
    }
  }
}
