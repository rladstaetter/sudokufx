package net.ladstatt.sudoku

import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  *
  * @param nr number of the frame
  */
case class SCandidate(nr: Int, framePipeline: FramePipeline) extends CanLog {

  import Parameters._

  private val corners: MatOfPoint2f = SudokuUtils.detectSudokuCorners(framePipeline.dilated)

  private val foundCorners: Boolean = !corners.empty

  private val destCorners: MatOfPoint2f = OpenCV.mkCorners(framePipeline.frame.size)
  private lazy val sudokuCanvas = OpenCV.warp(framePipeline.frame, corners, destCorners)
  private lazy val cellSize = mkCellSize(sudokuCanvas.size)
  private lazy val cellWidth = cellSize.width.toInt
  private lazy val cellHeight = cellSize.height.toInt

  private lazy val cellRects: Seq[Rect] = cellRange.map {
    case i => new Rect(col(i) * cellWidth, row(i) * cellHeight, cellWidth, cellHeight)
  }

  lazy val futureSCells: Seq[Future[SCell]] = cellRects.map(detectCell(TemplateLibrary.detectNumber, sudokuCanvas, _))
  private lazy val futureDetectedCells: Future[Seq[SCell]] = Future.fold(futureSCells)(Seq[SCell]())((cells, c) => cells ++ Seq(c))

  private lazy val sample: InputFrame = InputFrame(nr, framePipeline)

  /**
    * This function uses an input image and a detection method to calculate the sudoku.
    *
    * @param cap                number of detections for a certain number until it is regarded as "stable enough"
    * @param minHits            minimal number of numbers before a the solving is attempted
    * @param maxSolvingDuration number of milliseconds which the solver is given before he gives up
    */
  def calc(lastState: SudokuState,
           cap: Int,
           minHits: Int,
           maxSolvingDuration: Long): Future[(SudokuResult, SudokuState)] = {

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline

    if (foundCorners) {
      for {
        detectedCells <- futureDetectedCells
        mergedLibrary = SudokuUtils.mergeDigitLibrary(sudokuCanvas, lastState.library, detectedCells)
        hitsToCompute = SudokuUtils.mergeHits(lastState.hitCounts, detectedCells.map(_.value), cap)
        (someDigitSolution, someSolutionCells, currentState) <- SudokuUtils.computeSolution(hitsToCompute, mergedLibrary, cap, minHits, maxSolvingDuration)
        withSolution <- SudokuUtils.paintSolution(sudokuCanvas, detectedCells.map(_.value), someSolutionCells, currentState.library, cellRects)

        annotatedSolution <- SudokuUtils.paintCorners(withSolution, cellRects, someSolutionCells, currentState.hitCounts, cap)

        unwarped = OpenCV.warp(annotatedSolution, destCorners, corners)
        solutionMat <- OpenCV.copySrcToDestWithMask(unwarped, framePipeline.frame, unwarped) // copy solution mat to input mat
        sudokuFrame = SudokuFrame(sudokuCanvas, detectedCells.toArray, corners.toList.toList)
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
