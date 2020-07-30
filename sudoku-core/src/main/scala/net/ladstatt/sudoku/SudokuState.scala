package net.ladstatt.sudoku


import net.ladstatt.core.CanLog

object SudokuState {

  def apply(): SudokuState = {
    SudokuState(Seq.fill(Parameters.cellCount)(0), 1)
  }

  /**
   * Provide sudoku configuration in an 9x9 ascii matrix containing also newlines at the end
   * such that it is human readable.
   *
   * @param cellsWithNewlines 81 chars + 9 newlines
   */
  def apply(cellsWithNewlines: String, hitCount: Int): SudokuState = {
    SudokuState(cellsWithNewlines.filter(_.isDigit).map(_.asDigit), hitCount)
  }

  def apply(cells: Seq[Int], hitCount: Int): SudokuState = {
    SudokuState(cells, cells.map(number => Map(number -> hitCount)))
  }

}

case class SudokuState(cells: Seq[Int]
                       , hitHistory: Seq[Map[Int, Int]]) extends CanLog {

  //  assert(cells.size == Parameters.cellCount)

  /** converts cells to a hitcounter Seq which contains all cells and a hitcount set to minHitCount
   * in order to trigger a solution attempt with given numbers */
  def assumeReadyToSolve: SudokuState =
    SudokuState(cells, for {m <- hitHistory
                            (number, _) <- m} yield Map(number -> Sudoku.minNrOfValueHits))

  def printlnHitHistory(): Unit = {
    for ((c, i) <- hitHistory.zipWithIndex) {
      val maybeTuple = c.find { case (_, frequency) => frequency >= Sudoku.minNrOfValueHits }
      maybeTuple match {
        case Some((n, f)) =>
          println(s"$i hit: $n ($f)")
        case None =>
          println(s"$i candidate: $c")
      }
    }
  }

  /** analyses hits and returns numbers if threshold is reached, otherwise 0 */
  lazy val cellValues: Seq[Int] = hitHistory.map(optBestNumber)

  /** for each cell there is a value */
  //assert(currentValues.size == Parameters.cellCount)

  lazy val isSolved: Boolean = 405 == cellValues.sum

  /** number of hits and keeping information from previous runs in mind */
  lazy val nrHits: Int = cellValues.count(_ != 0)

  lazy val isReadyToSolve: Boolean = nrHits >= Sudoku.minNrOfDetectedCells

  /** adds given history to current one */
  def add(other: SudokuState): SudokuState = {
    SudokuState(other.cells, for ((current, solved) <- hitHistory zip other.hitHistory) yield {
      (for (i <- 1 to 9) yield {
        i -> (current.getOrElse(i, 0) + solved.getOrElse(i, 0))
      }).toMap
    })


  }

  lazy val justSolved: SudokuState = {
    val (x, _) = (solved.cellValues zip cellValues).map { case (a, b) => if (b == 0) {
      (a, b)
    } else (0, b)
    }.unzip
    SudokuState(x, Sudoku.minNrOfValueHits)
  }
  lazy val solved: SudokuState = {
    if (isSolved) {
      this
    } else
      SudokuUtils.solve(cellValues, Sudoku.maxSolvingDuration) match {
        case None =>
          logWarn("Could not solve sudoku.")
          this
        case Some(history) => history
      }
  }


  def asSudokuString: String = cellValues.sliding(9, 9).map(_.mkString).mkString("\n")

  /** returns best hit for a given history of a cell, if threshold is not reached it returns 0 */
  def optBestNumber(m: Map[Int, Int]): Int = {
    val (number, hitCount) = m.toSeq.sortWith((a, b) => a._2 > b._2).head
    if (hitCount >= Sudoku.minNrOfValueHits) {
      number
    } else 0
  }


}