package net.ladstatt.sudoku

import net.ladstatt.core.CanLog
import net.ladstatt.opencv.{Debug, OpenCV}
import org.opencv.core._

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object SCandidate {

  def apply(nr: Int, input: Mat): SCandidate = {
    val pipeline: FramePipeline = FramePipeline(input, SParams())
    new SCandidate(nr, pipeline, SRectangle(pipeline, pipeline.detectedRectangle.get))
  }

}

trait SResult

/**
  *
  * @param nr number of the frame
  */
case class SCandidate(nr: Int, framePipeline: FramePipeline, sRectangle: SRectangle) extends CanLog with SResult {

  /**
    * This function uses an input image and a detection method to calculate the sudoku.
    *
    */
  def calc(lastState: SudokuState): Future[(SudokuResult, SudokuState)] = {

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline

    if (Debug.Active) Debug.writeDebug(this)

    for {
      detectedCells <- sRectangle.detectedCells
      detectedCellValues = detectedCells.map(_.value)
      state = lastState.copy(
        library = SudokuUtils.mergeDigitLibrary(sRectangle.normalized, lastState.library, detectedCells),
        hitCounts = SudokuUtils.mergeHits(lastState.hitCounts, detectedCellValues))
      (someDigitSolution, someSolutionCells, currentState) <- Future(state.solveSudoku())
      withSolution <- SudokuUtils.paintSolution(sRectangle.normalized, detectedCellValues, someSolutionCells, currentState.library, sRectangle.cellRois)
      annotatedSolution <- SudokuUtils.paintCorners(withSolution, sRectangle.cellRois, someSolutionCells, currentState.hitCounts, lastState.cap)
      warped = OpenCV.warp(annotatedSolution, framePipeline.corners, sRectangle.detectedCorners)
      solutionMat <- OpenCV.copySrcToDestWithMask(warped, framePipeline.frame, warped) // copy solution mat to input mat
      sudokuFrame = SudokuFrame(sRectangle.normalized, detectedCells.toArray, sRectangle.detectedCorners.toList.toList)
    } yield {
      val someSolution =
        if (someSolutionCells.isDefined) {
          Some(SolutionFrame(someDigitSolution.get, solutionMat))
        } else None

      (SSuccess(this, sudokuFrame, someSolution), currentState)
    }
  }


}
