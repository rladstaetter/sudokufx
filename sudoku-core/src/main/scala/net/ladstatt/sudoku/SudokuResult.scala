package net.ladstatt.sudoku

import org.opencv.core.{Mat, Point}

sealed trait SudokuResult

case class SCorners(nr: Int,
                    frame: Mat,
                    start: Long,
                    imageIOChain: ImageIOChain,
                    sudokuCanvas: VideoInput,
                    detectedCells: Cells,
                    sudokuCorners: List[Point]) extends SudokuResult

case class SFailure(nr: Int, frame: Mat, start: Long, imageIoChain: ImageIOChain) extends SudokuResult

case class SSuccess(nr: Int,
                    frame: Mat,
                    start: Long,
                    imageIOChain: ImageIOChain,
                    sudokuCanvas: VideoInput,
                    foundCorners: Boolean,
                    cells: Cells,
                    solution: SudokuDigitSolution,
                    solutionMat: Mat,
                    sudokuCorners: List[Point]) extends SudokuResult {

  lazy val detectedCells = cells.filter(_.value != 0)

  def solutionAsString: String = solution.sliding(9, 9).map(new String(_)).mkString("\n")
}
