package net.ladstatt.apps.sudoku

import org.opencv.core.{Mat, Point}

sealed trait SudokuResult

case class SCorners(nr: Int,
                    frame: Mat,
                    start: Long,
                    imageIOChain: ImageIOChain,
                    sudokuCanvas: SudokuCanvas,
                    detectedCells: Cells,
                    sudokuCorners: List[Point]) extends SudokuResult

case class SFailure(nr: Int, frame: Mat, start: Long, imageIoChain: ImageIOChain) extends SudokuResult

case class SSuccess(nr: Int,
                    frame: Mat,
                    start: Long,
                    imageIOChain: ImageIOChain,
                    sudokuCanvas: SudokuCanvas,
                    foundCorners: Boolean,
                    detectedCells: Cells,
                    solution: SudokuDigitSolution,
                    solutionMat: Mat,
                    sudokuCorners: List[Point]) extends SudokuResult {

  def solutionAsString: String = solution.sliding(9, 9).map(new String(_)).mkString("\n")
}
