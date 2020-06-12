package net.ladstatt.sudoku

import org.bytedeco.opencv.opencv_core.Mat

import scala.util.{Failure, Success, Try}


/**
 *
 * @param nr number of the frame
 */
case class SCandidate(nr: Int
                      , pipeline: FramePipeline
                      , sCanvas: SudokuCanvas
                      , currentState: SudokuState) extends SResult {

  lazy val calc: SudokuResult = {
    Try {
      val solvedState: SudokuState = currentState.update(sCanvas).solve()
      for (r <- solvedState.someResult) yield {
        val withSolution: Mat = sCanvas.paintSolution(solvedState.someCells, solvedState.library)
        val annotatedSolution: Mat = SudokuUtils.paintCorners(withSolution, sCanvas.cellRois, solvedState.someCells, solvedState.hitCounts, currentState.cap)
        val warped: Mat = JavaCV.unwarp(annotatedSolution, sCanvas.detectedCorners)
        val solutionMat: Mat = FramePipeline.copySrcToDestWithMask(warped, sCanvas.inputFrame, warped)
        SolutionFrame(r, solvedState, solutionMat)
      }
    } match {
      case Failure(exception) =>
        SFailure(this, exception.getMessage)
      case Success(solution) =>
        SSuccess(this, sCanvas, solution)
    }
  }

}
