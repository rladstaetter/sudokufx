package net.ladstatt.sudoku

import scala.util.{Failure, Success, Try}

object SResult {

  def apply(index: Int
            , oldState: SudokuState
            , sudoku: Sudoku
           ): SResult = {
    apply(index, oldState, sudoku.contourParams, sudoku.pipeline)
  }

  def apply(index: Int
            , oldState: SudokuState
            , params: ContourParams
            , pipeline: FramePipeline): SResult = {
    val res: SResult =
      Try(SudokuUtils.detectSudokuCanvas(params)(pipeline) match {
        case None => pipeline
        case Some(sudokuCanvas) =>
          // JavaCV.drawContours(pipeline.frame, sudokuCanvas.detectedCorners)
          SCandidate(index, pipeline, sudokuCanvas, oldState)
      }) match {
        case Success(v) => v
        case Failure(e) =>
          e.printStackTrace()
          ???
      }
    res
  }
}

trait SResult
