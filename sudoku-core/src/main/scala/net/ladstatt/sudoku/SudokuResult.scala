package net.ladstatt.sudoku


import org.bytedeco.opencv.opencv_core.Mat


case class InputFrame(nr: Int, framePipeline: FramePipeline)

case class SolutionFrame(solution: SudokuDigitSolution
                         , solvedState: SudokuState
                         , solutionMat: Mat) {
  def solutionAsString: String = solution.sliding(9, 9).map(new String(_)).mkString("\n")
}

sealed trait SudokuResult {
  def candidate: SCandidate
}

/**
  * Represents a valid sudoku detection, possibly no solution.
  *
  * @param candidate
  * @param sudokuFrame
  * @param someSolution
  */
case class SSuccess(candidate: SCandidate,
                    sudokuFrame: SudokuCanvas,
                    someSolution: Option[SolutionFrame]) extends SudokuResult

case class SFailure(candidate: SCandidate, msg: String) extends SudokuResult


