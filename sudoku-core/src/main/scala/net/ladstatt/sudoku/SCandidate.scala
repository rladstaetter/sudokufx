package net.ladstatt.sudoku

import net.ladstatt.core.CanLog
import net.ladstatt.opencv.{Debug, OpenCV}
import org.opencv.core._

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object SCandidate {

  def apply(nr: Int, input: Mat, state: SudokuState): SCandidate = {
    val pipeline: FramePipeline = FramePipeline(input, SParams())
    new SCandidate(nr, pipeline, SRectangle(pipeline.frame, pipeline.detectedRectangle.get, pipeline.corners), state)
  }

}

trait SResult

/**
  *
  * @param nr number of the frame
  */
case class SCandidate(nr: Int,
                      framePipeline: FramePipeline,
                      sRectangle: SRectangle,
                      oldState: SudokuState) extends CanLog with SResult {

  if (Debug.Active) Debug.writeDebug(this)

  /**
    * This function uses an input image and a detection method to calculate the sudoku.
    *
    */
  lazy val calc: Future[(SudokuResult, SudokuState)] = {
    for {
      detectedCells <- sRectangle.detectedCells
      detectedCellValues = detectedCells.map(_.value)
      currentState = oldState.merge(sRectangle.normalized, detectedCells, detectedCellValues)
      solvedState <- Future(currentState.solve())
      withSolution <- sRectangle.paintSolution(detectedCellValues, solvedState.someCells, currentState.library)
      annotatedSolution <- SudokuUtils.paintCorners(withSolution, sRectangle.cellRois, solvedState.someCells, currentState.hitCounts, oldState.cap)
      warped = OpenCV.warp(annotatedSolution, framePipeline.corners, sRectangle.detectedCorners)
      solutionMat <- OpenCV.copySrcToDestWithMask(warped, framePipeline.frame, warped) // copy solution mat to input mat
      sudokuFrame = SudokuFrame(sRectangle.normalized, detectedCells.toArray, sRectangle.detectedCorners.toList.toList)
    } yield {
      (SSuccess(this, sudokuFrame, solvedState.someResult.map(s => SolutionFrame(s, solutionMat))), currentState)
    }
  }

}
