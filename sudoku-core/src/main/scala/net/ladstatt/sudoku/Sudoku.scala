package net.ladstatt.sudoku

import java.nio.file.{Path, Paths}

import net.ladstatt.sudoku.JavaCV._
import org.bytedeco.opencv.opencv_core.{Mat, Rect}

object Sudoku {

  val targetPath: Path = Paths.get("target/sessions").resolve(System.currentTimeMillis().toString)

  /** if set to true, take input from testsession */
  val debug = false

  /** if set to true, sudokufx will write debug files / images */
  val writeFiles = true

  /** don't try to solve sudoku until a certain amount of cells containing numbers are detected */
  val minNrOfDetectedCells = 19

  /**
   * for a given cell, detect at least minNrOfValueHits times a number 'to be sure' that it is really
   * the number we are after
   **/
  val minNrOfValueHits = 10

  /* a hit has to reach this quality in order to be used as a value */
  val minQuality = 15500000
  //val minQuality = 12500000
  //val minQuality = 3500000

}

/**
 * Created by lad on 01.05.16.
 *
 * @param frame      video input
 * @param detectedCorners corner points of detected sudoku
 */
case class Sudoku(id: String
                  , frameNr: Int
                  , frame: Mat
                  , normalized: Mat
                  , corners: Seq[Float]
                  , detectedCorners: Mat
                  , sHistory: SudokuHistory
                  , digitLibrary: DigitLibrary) {

  val isSolved: Boolean = sHistory.isSolved

  /**
   * the cellRois denote the region of interests for every sudoku cell (there are 81 of them for every sudoku)
   */
  val cellRois: Seq[Rect] = Parameters.cellRange.map(JavaCV.mkRect(_, JavaCV.mkCellSize(normalized.size)))

  val cells: Seq[SCell] = cellRois.zipWithIndex.map {
    case (r, i) => SCell(id, frameNr, i, normalized.apply(r), r, sHistory.cells(i))
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


  // implicitly perform all calculations, provide new image recognition situation
  lazy val trySolve: Option[SolvedSudoku] = {
    // logInfo(cells.size.toString)
    // println("-" * 80)
    // println("- Former hit history")
    // sHistory.printlnHitHistory()
    //println("-" * 80)
    //println("- Updated hit history")
    //updatedHitHistory.printlnHitHistory()
    //println("- Updated hit history")
    //println("-" * 80)
    // change templates to the better
    val updatedLibrary = SudokuUtils.mergeDigitLibrary(normalized, digitLibrary, cells)
    logInfo("NrDetections: " + updatedHitHistory.nrHits + " minHits: " + Sudoku.minNrOfDetectedCells)
    if (updatedHitHistory.isReadyToSolve) {
      logInfo("Trying to solve:")
      logInfo("\n" + updatedHitHistory.asSudokuString)
      val solvedSudoku = updatedHitHistory.solved
      if (solvedSudoku.isSolved) {
        Option(SolvedSudoku(frameNr, frame, normalized, detectedCorners, cellRois, solvedSudoku, updatedLibrary))
      } else {
        logWarn("Could not verify solution, resetting ...")
        /*        Option(SolvedSudoku(inputFrame
                  , detectedCorners
                  , SudokuHistory()
                  , updatedLibrary))

         */
        None
      }
    } else {
      logTrace("did not yet find enough hits to solve sudoku")
      Option(SolvedSudoku(frame
        , detectedCorners
        , updatedHitHistory
        , updatedLibrary))
    }
  }


}

object SolvedSudoku {

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

  def apply(frameNr: Int
            , inputFrame: Mat
            , normalized: Mat
            , detectedCorners: Mat
            , cellRois: Seq[Rect]
            , solvedSudoku: SudokuHistory
            , updatedLibrary: DigitLibrary): SolvedSudoku = {
    val cNormalized: Mat = normalizeCanvas(normalized, solvedSudoku.currentValues, cellRois, updatedLibrary)
    // val cells: Seq[SCell] = SudokuUtils.toSolutionCells(updatedLibrary, solvedSudoku)
    JavaCV.writeMat(Sudoku.targetPath.resolve(s"$frameNr-solvedCanvasNormalized.png"), cNormalized)

    val warped: Mat = JavaCV.unwarp(cNormalized, detectedCorners)
    JavaCV.writeMat(Sudoku.targetPath.resolve(s"$frameNr-warped.png"), warped)
    val solutionMat: Mat = JavaCV.copySrcToDestWithMask(warped, inputFrame, warped)
    JavaCV.writeMat(Sudoku.targetPath.resolve(s"$frameNr-solution.png"), solutionMat)

    SolvedSudoku(solutionMat
      , detectedCorners
      , solvedSudoku
      , updatedLibrary)
  }

}

case class SolvedSudoku(video: Mat
                        , detectedCorners: Mat
                        , sudokuHistory: SudokuHistory
                        , library: DigitLibrary)
