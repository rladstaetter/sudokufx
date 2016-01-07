package net.ladstatt.sudoku

import java.util

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
  * @param frame the frame information itself
  */
case class SCandidate(nr: Int, frame: Mat) extends CanLog {

  val start = System.nanoTime()

  private val imageIoChain: ImageIOChain = ImageIOChain(frame)

  private val cornerDetector: CornerDetector = CornerDetector(imageIoChain.dilated)

  private lazy val warper = Warper(frame, cornerDetector.corners)
  private lazy val cellDetector: CellDetector = CellDetector(warper.sudokuCanvas)
  private lazy val sample: InputFrame = InputFrame(nr, frame, start, imageIoChain)

  // TODO remove
  def corners: util.List[Point] = cornerDetector.corners.toList

  def futureSCells = cellDetector.futureSCells


  /**
    * This function uses an input image and a detection method to calculate the sudoku.
    *
    * @param cap number of detections for a certain number until it is regarded as "stable enough"
    * @param minHits minimal number of numbers before a the solving is attempted
    * @param maxSolvingDuration number of milliseconds which the solver is given before he gives up
    */
  def calc(lastDigitLibrary: DigitLibrary,
           lastHits: HitCounters,
           cap: Int,
           minHits: Int,
           maxSolvingDuration: Long): Future[(SudokuResult, DigitLibrary, HitCounters)] = {

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline

    if (cornerDetector.foundCorners) {
      for {
        detectedCells <- cellDetector.futureDetectedCells
        mergedLibrary = SudokuUtils.mergeDigitLibrary(warper.sudokuCanvas, lastDigitLibrary, detectedCells)
        hitsToCompute = SudokuUtils.mergeHits(lastHits, detectedCells.map(_.value), cap)

        (someDigitSolution, someSolutionCells, currentHits, currentDigitLibrary) <- SudokuUtils.computeSolution(hitsToCompute, mergedLibrary, cap, minHits, maxSolvingDuration)

        withSolution <- SudokuUtils.paintSolution(cellDetector.sudokuCanvas,
          detectedCells.map(_.value),
          someSolutionCells,
          currentDigitLibrary,
          cellDetector.cellRects)

        annotatedSolution <- SudokuUtils.paintCorners(withSolution, cellDetector.cellRects, someSolutionCells, currentHits, cap)

        unwarped = OpenCV.warp(annotatedSolution, mkCorners(frame.size), cornerDetector.corners)
        solutionMat <- OpenCV.copySrcToDestWithMask(unwarped, frame, unwarped) // copy solution mat to input mat
        sudokuFrame = SudokuFrame(warper.sudokuCanvas, detectedCells.toArray, cornerDetector.corners.toList.toList)
      } yield {

        val someSolution =
          if (someSolutionCells.isDefined) {
            Some(SolutionFrame(someDigitSolution.get, solutionMat))
          } else None

        (SSuccess(sample, sudokuFrame, someSolution), currentDigitLibrary, currentHits)
      }
    } else {
      Future.successful((SFailure("Couldn't detect corners", sample), lastDigitLibrary, lastHits))
    }

  }


}
