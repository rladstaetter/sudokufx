package net.ladstatt.sudoku


import net.ladstatt.core.CanLog

object SudokuHistory {

  def apply(): SudokuHistory = {
    SudokuHistory(Seq.fill(Parameters.cellCount)(0), 1)
  }

  /**
   * Provide sudoku configuration in an 9x9 ascii matrix containing also newlines at the end
   * such that it is human readable.
   *
   * @param cellsWithNewlines 81 chars + 9 newlines
   */
  def apply(cellsWithNewlines: String, hitCount: Int): SudokuHistory = {
    SudokuHistory(cellsWithNewlines.filter(_.isDigit).map(_.asDigit), hitCount)
  }

  def apply(cells: Seq[Int], hitCount: Int): SudokuHistory = {
    SudokuHistory(0, cells.map(number => Map(number -> hitCount)))
  }

}

case class SudokuHistory(timestamp: Long
                         , cells: Seq[Map[Int, Int]]) extends CanLog {

  assert(cells.size == Parameters.cellCount)

  /** converts cells to a hitcounter Seq which contains all cells and a hitcount set to minHitCount
   * in order to trigger a solution attempt with given numbers */
  def assumeReadyToSolve =
    SudokuHistory(timestamp, for {m <- cells
                                  (number, _) <- m} yield Map(number -> Sudoku.minNrOfValueHits))

  def printlnHitHistory(): Unit = {
    for ((c, i) <- cells.zipWithIndex) {
      println(s"$i $c")
    }
  }

  /** analyses hits and returns numbers if threshold is reached, otherwise 0 */
  lazy val currentValues: Seq[Int] = cells.map(optBestNumber)

  /** for each cell there is a value */
  //assert(currentValues.size == Parameters.cellCount)

  lazy val isSolved: Boolean = 405 == currentValues.sum

  lazy val nrHits: Int = currentValues.count(_ != 0)

  lazy val isReadyToSolve: Boolean = nrHits > Sudoku.minNrOfDetectedCells

  /** adds given history to current one */
  def add(other: SudokuHistory): SudokuHistory = {
    SudokuHistory(0, for ((current, solved) <- cells zip other.cells) yield {
      (for (i <- 1 to 9) yield {
        i -> (current.getOrElse(i, 0) + solved.getOrElse(i, 0))
      }).toMap
    })


  }

  lazy val solved: SudokuHistory = {
    if (isSolved) {
      this
    } else
      SudokuUtils.solve(currentValues.map(x => (x + 48).toChar).toArray, Parameters.maxSolvingDuration) match {
        case None =>
          logWarn("Could not solve sudoku.")
          this
        case Some(solution) =>
          logTrace("Solved sudoku")
          val solutionAsDigits: Seq[Int] = solution.map(_.asDigit).toSeq
          SudokuHistory(solutionAsDigits, Sudoku.minNrOfValueHits)
        //          add(solvedSudoku)
      }
  }

  def asSudokuString: String = currentValues.sliding(9, 9).map(_.mkString).mkString("\n")

  /** returns best hit for a given history of a cell, if threshold is not reached it returns 0 */
  def optBestNumber(m: Map[Int, Int]): Int = {
    val (number, hitCount) = m.toSeq.sortWith((a, b) => a._2 > b._2).head
    if (hitCount >= Sudoku.minNrOfValueHits) {
      number
    } else 0
  }

  /*
  def optBestNumber(m: Map[Int, Int]): Int =
    m.toSeq.sortWith((a, b) => a._2 > b._2).
      headOption.filter {
      case (_, hits) => hits >= Sudoku.minNrOfValueHits
    }.mdap(_._1).getOrElse(0)
*/
}