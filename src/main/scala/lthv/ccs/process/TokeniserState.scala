package lthv.ccs.process

import lthv.ccs.graph.State
import lthv.ccs.graph.State.START
import lthv.ccs.token.{FileState, MaterialisedToken}

case class TokeniserState(
  state: State,
  fileState: FileState
) {

  def materialiseToken: MaterialisedToken = state.materialiseToken(fileState.value, fileState)

  def nextNonMaterialisedState(nextState: State, c: Char): TokeniserState = {
    this.copy(
      state = nextState,
      fileState = this.fileState.copy(
        value = this.fileState.value + c,
        iCharEnd = this.fileState.iCharEnd + 1
      )
    )
  }
}

object TokeniserState {

  def apply(line: Int): TokeniserState = new TokeniserState(START, FileState("", line, 0, 0))

  def getStateAfterFailure(tokeniserState: TokeniserState): TokeniserState = {

    val previousFileState = tokeniserState.fileState

    new TokeniserState(START, FileState("", previousFileState.line, previousFileState.iCharEnd + 1, previousFileState.iCharEnd + 1))
  }

  def apply(previousState: TokeniserState, readChar: Char, currentState: State): TokeniserState = {

    val previousFileState = previousState.fileState

    new TokeniserState(
      currentState,
      FileState(readChar.toString, previousFileState.line, previousFileState.iCharEnd, previousFileState.iCharEnd + 1)
    )
  }
}
