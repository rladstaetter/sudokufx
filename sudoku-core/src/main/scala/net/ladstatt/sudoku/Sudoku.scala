package net.ladstatt.sudoku

import java.nio.FloatBuffer
import java.nio.file.Paths

import net.ladstatt.sudoku.JavaCV._
import org.bytedeco.opencv.opencv_core.{Mat, Rect}

import scala.concurrent.duration.FiniteDuration

object Sudoku {

  /** don't try to solve sudoku until a certain amount of numbers are detected */
  val minNrOfDetectedCells = 22

  /**
   * for a given cell, detect at least minNrOfValueHits times a number 'to be sure' that it is really
   * the number we are after
   **/
  val minNrOfValueHits = 20


}

/**
 * Created by lad on 01.05.16.
 *
 * @param inputFrame      video input
 * @param detectedCorners corner points of detected sudoku
 */
case class Sudoku(id: String
                  , nr: Int
                  , inputFrame: Mat
                  , detectedCorners: Mat
                  , sHistory: SudokuHistory
                  , digitLibrary: DigitLibrary
                  , cap: Int = Parameters.cap
                  , minHits: Int = Parameters.minHits
                  , maxSolvingDuration: FiniteDuration = Parameters.maxSolvingDuration) {

  /**
   * This mat contains an 'unstretched' version of the detected sudoku outer rectangle.
   *
   * In this representation it is easier to paint upon. After painting this Mat will be retransformed
   * to the original appearance again.
   */
  val normalized: Mat = JavaCV.warp(inputFrame, detectedCorners)

  /**
   * the cellRois denote the region of interests for every sudoku cell (there are 81 of them for every sudoku)
   */
  val cellRois: Seq[Rect] = Parameters.cellRange.map(JavaCV.mkRect(_, JavaCV.mkCellSize(normalized.size)))

  val cells: Seq[SCell] = cellRois.zipWithIndex.map {
    case (r, i) => SCell(id, i, normalized.apply(r), r, sHistory.cells(i))
  }


  /** contains hithistory */
  val updatedHitHistory: SudokuHistory = SudokuHistory(0, cells.map(_.updatedHits))

  /**
   * paints the solution to the canvas.
   *
   * returns the modified canvas with the solution painted upon.
   *
   * detectedCells contains values from 0 to 9, with 0 being the cells which are 'empty' and thus have to be filled up
   * with numbers.
   *
   * uses digitData as lookup table to paint onto the canvas, thus modifying the canvas.
   */
  // lazy val solvedCanvasNormalized: Mat = ssolvedCanvasNormalized(normalized, cellValues, cellRois)


  // lazy val annotated = SudokuUtils.paintCorners(solvedCanvasNormalized, cellRois, hitHistory, cap)

  /* sudoku outer rectangle */
  lazy val corners: Seq[Float] = {
    val bf = detectedCorners.createBuffer[FloatBuffer]
    val (x1, y1) = (bf.get(0), bf.get(1))
    val (x2, y2) = (bf.get(2), bf.get(3))
    val (x3, y3) = (bf.get(4), bf.get(5))
    val (x4, y4) = (bf.get(6), bf.get(7))
    Seq(x1, y1
      , x2, y2
      , x3, y3
      , x4, y4)
  }


  // implicitly perform all calculations, provide new image recognition situation
  lazy val trySolve: Option[SolvedSudoku] = {
    // logInfo(cells.size.toString)
    // println("-" * 80)
    // println("- Former hit history")
    // sHistory.printlnHitHistory()
    println("-" * 80)
    println("- Updated hit history")
    updatedHitHistory.printlnHitHistory()
    // change templates to the better
    val updatedLibrary = SudokuUtils.mergeDigitLibrary(normalized, digitLibrary, cells)
    logInfo("NrDetections: " + updatedHitHistory.nrHits + " minHits: " + minHits)
    if (updatedHitHistory.isReadyToSolve) {
      val solvedSudoku = updatedHitHistory.solved
      if (solvedSudoku.isSolved) {
        val cNormalized: Mat = normalizeCanvas(normalized, solvedSudoku.currentValues, cellRois, updatedLibrary)
        // val cells: Seq[SCell] = SudokuUtils.toSolutionCells(updatedLibrary, solvedSudoku)
        JavaCV.writeMat(Paths.get(s"target/sudoku-$nr-solvedCanvasNormalized.png"), cNormalized)

        val warped: Mat = JavaCV.unwarp(cNormalized, detectedCorners)
        JavaCV.writeMat(Paths.get(s"target/sudoku-$nr-warped.png"), warped)
        val solutionMat: Mat = JavaCV.copySrcToDestWithMask(warped, inputFrame, warped)

        Option(SolvedSudoku(solutionMat
          , detectedCorners
          , solvedSudoku
          , updatedLibrary))
      } else {
        logWarn("Could not verify solution, resetting ...")
        Option(SolvedSudoku(inputFrame
          , detectedCorners
          , SudokuHistory()
          , updatedLibrary))
        None
      }
    } else {
      logInfo("did not yet find enough hits to solve sudoku")
      Option(SolvedSudoku(inputFrame
        , detectedCorners
        , updatedHitHistory
        , updatedLibrary))
    }
  }

  def normalizeCanvas(normalized: Mat
                      , cellValues: Seq[Int]
                      , cellRois: Seq[Rect]
                      , digitLibrary: DigitLibrary): Mat = {
    for ((s, r) <- cellValues zip cellRois if s != 0) {
      val someNormalizedCell: Option[Mat] =
        for {d <- digitLibrary(s)._2
             fb <- SudokuUtils.mkFallback(s, digitLibrary)} yield fb
      someNormalizedCell foreach {
        normalizedCell => copyTo(normalized, normalizedCell, r)
      }
    }
    normalized
  }

}

case class SolvedSudoku(video: Mat
                        , detectedCorners: Mat
                        , sudokuHistory: SudokuHistory
                        , library: DigitLibrary)
