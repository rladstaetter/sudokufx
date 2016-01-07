package net.ladstatt.sudoku

import org.opencv.core.{Mat, Point}

case class InputFrame(nr: Int, frame: Mat, start: Long, imageIOChain: ImageIOChain)

case class SudokuFrame(in: Mat, cells: Cells, corners: List[Point]) {
  lazy val detectedCells = cells.filter(_.value != 0)
}

case class SolutionFrame(solution: SudokuDigitSolution, solutionMat: Mat) {
  def solutionAsString: String = solution.sliding(9, 9).map(new String(_)).mkString("\n")
}

sealed trait SudokuResult {
  def inputFrame: InputFrame
}

/**
  * Represents a valid sudoku detection, possibly no solution.
  *
  * @param inputFrame
  * @param sudokuFrame
  * @param someSolution
  */
case class SSuccess(inputFrame: InputFrame,
                    sudokuFrame: SudokuFrame,
                    someSolution: Option[SolutionFrame]) extends SudokuResult

case class SFailure(msg: String, inputFrame: InputFrame) extends SudokuResult


