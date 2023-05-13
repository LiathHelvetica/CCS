package lthv.ccs.preprocess

object Preprocessor {

  def removeComments(contents: Seq[String]): Seq[String] = {
    contents.map(line => line.split("\\\\")(0))
  }
}
