package net.ladstatt.sudoku
import java.nio.file.Path

import net.ladstatt.sudoku.JavaCV._
import org.bytedeco.opencv.opencv_core.{Mat, Rect}

import scala.language.postfixOps

object SolvedSudoku {

  def populateWithSolution(normalized: Mat
                           , sudokuState: SudokuState
                           , cellRois: Seq[Rect]
                           , digitLibrary: DigitLibrary): Mat = {
    ((sudokuState.cells zip sudokuState.cellValues) zip cellRois).foldLeft(normalized) {
      case (norm, ((orig, detectedValue), r)) =>
        if (orig != 0) {
          norm
        } else {
          val cell = {
            if (digitLibrary.contains(detectedValue)) {
              digitLibrary.digits(detectedValue).optMat match {
                case Some(s) => s
                case None => SudokuUtils.mkFallback(detectedValue, digitLibrary).get
              }
            } else {
              SudokuUtils.mkFallback(detectedValue, digitLibrary).get
            }
          }
          copyTo(norm, cell, r)
        }
    }
  }

  def apply(frameNr: Int
            , inputFrame: Mat
            , normalized: Mat
            , detectedCorners: Mat
            , cellRois: Seq[Rect]
            , sudokuState: SudokuState
            , digitLibrary: DigitLibrary
            , targetPath: Path): SolvedSudoku = {
    val sudokuNormalizedWithSolution: Mat = populateWithSolution(normalized, sudokuState, cellRois, digitLibrary)
    // paintCorners(sudokuNormalizedWithSolution, cellRois, sudokuState.hitHistory, 4)

    JavaCV.writeMat(targetPath.resolve(s"$frameNr-solvedCanvasNormalized.png"), sudokuNormalizedWithSolution)

    val warped: Mat = JavaCV.unwarp(sudokuNormalizedWithSolution, detectedCorners)
    JavaCV.writeMat(targetPath.resolve(s"$frameNr-warped.png"), warped)
    val solutionMat: Mat = JavaCV.copySrcToDestWithMask(warped, inputFrame, warped)
    JavaCV.writeMat(targetPath.resolve(s"$frameNr-solution.png"), solutionMat)

    SolvedSudoku(solutionMat
      , Option(sudokuNormalizedWithSolution)
      , detectedCorners
      , sudokuState)
  }

}

case class SolvedSudoku(frameWithSolution: Mat
                        , optCNormalized: Option[Mat]
                        , detectedCorners: Mat
                        , sudokuState: SudokuState)