package lthv.ccs.preprocess

object Preprocessor {

  def reduceRedundantLines(contents: Seq[(String, Int)]): Seq[(String, Int)] = {
    contents.foldLeft(Seq.empty[(String, Int)])((acc, line) => line._1.split("\\\\")(0)  match {
      case l if l.isBlank => acc  // empty lines and whitespaced comments - removed
      case l => acc :+ (l -> line._2)
    })
  }
}
