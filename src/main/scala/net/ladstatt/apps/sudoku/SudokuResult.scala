package net.ladstatt.apps.sudoku

import org.opencv.core.{Point, Mat}

/**
 * Created by lad on 07.01.15.
 */
sealed trait SudokuResult {
  def candidate: SCandidate
}

/**
 * Created by lad on 07.01.15.
 */
case class SFailure(candidate: SCandidate) extends SudokuResult

/**
 * Created by lad on 07.01.15.
 */
case class SSuccess(candidate: SCandidate,
                    detectedCells: Cells,
                    solution: SudokuDigitSolution,
                    solutionMat: Mat,
                    solutionCells: Cells,
                    sudokuCorners: List[Point]) extends SudokuResult {

  def solutionAsString: String = solution.sliding(9, 9).map(new String(_)).mkString("\n")
}
