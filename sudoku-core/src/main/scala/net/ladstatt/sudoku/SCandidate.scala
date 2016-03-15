package net.ladstatt.sudoku

import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object SCandidate {

  def apply(nr: Int, input: Mat): SCandidateImpl = {
    null
  }

}

trait SCandidate {
  def calc(lastState: SudokuState): Future[(SudokuResult, SudokuState)]

  def contours: Seq[MatOfPoint]
}


/**
  *
  * @param nr number of the frame
  */
case class SCandidateImpl(nr: Int, framePipeline: FramePipeline, params: SParams = SParams()) extends CanLog with SCandidate {

  private val destCorners: MatOfPoint2f = OpenCV.mkCorners(framePipeline.dilated.size)
  val (contours, someDetectedCorners) = SudokuUtils.detectSudokuCorners(framePipeline.dilated, params)

  private lazy val sudokuCanvas = OpenCV.warp(framePipeline.frame, someDetectedCorners.get, destCorners)
  private lazy val cellSize = mkCellSize(sudokuCanvas.size)
  private lazy val cellWidth = cellSize.width.toInt
  private lazy val cellHeight = cellSize.height.toInt

  private lazy val cellRects: Seq[Rect] = Parameters.cellRange.map {
    case i => new Rect(Parameters.col(i) * cellWidth, Parameters.row(i) * cellHeight, cellWidth, cellHeight)
  }

  lazy val futureSCells: Seq[Future[SCell]] = cellRects.map(detectCell(TemplateLibrary.detectNumber, sudokuCanvas, _))
  private lazy val futureDetectedCells: Future[Seq[SCell]] = Future.fold(futureSCells)(Seq[SCell]())((cells, c) => cells ++ Seq(c))

  private lazy val sample: InputFrame = InputFrame(nr, framePipeline)

  /**
    * This function uses an input image and a detection method to calculate the sudoku.
    *
    */
  def calc(lastState: SudokuState): Future[(SudokuResult, SudokuState)] = {

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline

    if (!someDetectedCorners.get.toList.isEmpty) {
      for {
        detectedCells <- futureDetectedCells
        detectedCellValues = detectedCells.map(_.value)
        state =
        lastState.copy(library = SudokuUtils.mergeDigitLibrary(sudokuCanvas, lastState.library, detectedCells),
          hitCounts = SudokuUtils.mergeHits(lastState.hitCounts, detectedCellValues))
        (someDigitSolution, someSolutionCells, currentState) <- Future {
          state.solveSudoku()
        }
        withSolution <- SudokuUtils.paintSolution(sudokuCanvas, detectedCellValues, someSolutionCells, currentState.library, cellRects)
        annotatedSolution <- SudokuUtils.paintCorners(withSolution, cellRects, someSolutionCells, currentState.hitCounts, lastState.cap)
        unwarped = OpenCV.warp(annotatedSolution, destCorners, someDetectedCorners.get)
        solutionMat <- OpenCV.copySrcToDestWithMask(unwarped, framePipeline.frame, unwarped) // copy solution mat to input mat
        sudokuFrame = SudokuFrame(sudokuCanvas, detectedCells.toArray, someDetectedCorners.get.toList.toList)
      } yield {
        val someSolution =
          if (someSolutionCells.isDefined) {
            Some(SolutionFrame(someDigitSolution.get, solutionMat))
          } else None

        (SSuccess(sample, sudokuFrame, someSolution), currentState)
      }
    } else {
      Future.successful((SFailure("Couldn't detect corners", sample), lastState))
    }

  }


}
