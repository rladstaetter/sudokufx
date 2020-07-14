package net.ladstatt.sudoku

import java.nio.FloatBuffer
import java.nio.file.{Path, Paths}

import net.ladstatt.sudoku.JavaCV._
import org.bytedeco.opencv.opencv_core.{Mat, Rect}

import scala.concurrent.duration._
import scala.language.postfixOps

object Sudoku {

  val targetPath: Path = Paths.get("target/sessions").resolve(System.currentTimeMillis().toString)
  /**
   * the maximum time the algorithm should search for a solution
   */
  val maxSolvingDuration: FiniteDuration = 1000 millis

  /** if set to true, take input from testsession */
  val debug = false

  /** if set to true, sudokufx will write debug files / images */
  val writeFiles = false

  /** don't try to solve sudoku until a certain amount of cells containing numbers are detected */
  val minNrOfDetectedCells = 25

  /**
   * for a given cell, detect at least minNrOfValueHits times a number 'to be sure' that it is really
   * the number we are after
   **/
  val minNrOfValueHits = 20

  /* a hit has to reach this quality in order to be used as a value */
  val minQuality = 20000000
  //val minQuality = 15500000
  // val minQuality = 12500000
  //val minQuality = 3500000

  def apply(id: String
            , frameNr: Int
            , frame: Mat
            , detectedCorners: Mat
            , history: SudokuHistory
            , library: DigitLibrary): Sudoku = {
    val normalized = JavaCV.warp(frame, detectedCorners)
    /*
    if ((frameNr % 25) == 0) {
      JavaCV.writeMat(targetPath.resolve(frameNr + "-frame.png"), frame)
      JavaCV.writeMat(targetPath.resolve(frameNr + "-normalized.png"), normalized)
      JavaCV.writeMat(Sudoku.targetPath.resolve(frameNr + "-dilated.png"), dilated)
    }
    */
    /* sudoku outer rectangle */
    val corners: Seq[Float] = {
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
    /**
     * the cellRois denote the region of interests for every sudoku cell (there are 81 of them for every sudoku)
     */
    val cellRois: Seq[Rect] = Parameters.cellRange.map(JavaCV.mkRect(_, JavaCV.mkCellSize(normalized.size)))

    val cells: Seq[SCell] = cellRois.zipWithIndex.map {
      case (r, i) => SCell(id, frameNr, i, normalized.apply(r), r, history.cells(i))
    }
    val updatedLibrary = library.add(normalized, cells.filter(_.detectedValue != 0))
    val updatedHistory = history.add(SudokuHistory(0, cells.map(_.updatedHits)))

    Sudoku(id
      , frameNr
      , frame
      , normalized
      , corners
      , detectedCorners
      , cellRois
      , cells
      , updatedHistory
      , updatedLibrary)
  }
}

/**
 * Created by lad on 01.05.16.
 *
 * @param frame           video input
 * @param detectedCorners corner points of detected sudoku
 */
case class Sudoku(id: String
                  , frameNr: Int
                  , frame: Mat
                  , normalized: Mat
                  , corners: Seq[Float]
                  , detectedCorners: Mat
                  , cellRois: Seq[Rect]
                  , cells: Seq[SCell]
                  , history: SudokuHistory
                  , digitLibrary: DigitLibrary) {

  val isSolved: Boolean = history.isSolved


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

    logInfo("NrDetections: " + history.nrHits + " minHits: " + Sudoku.minNrOfDetectedCells)
    if (history.isReadyToSolve) {
      logInfo("Trying to solve:")
      logInfo("\n" + history.asSudokuString)
      val solvedSudoku: SudokuHistory = history.solved
      if (solvedSudoku.isSolved) {
        Option(SolvedSudoku(frameNr, frame, normalized, detectedCorners, cellRois, solvedSudoku, digitLibrary))
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
        , None
        , detectedCorners
        , history))
    }
  }


}

object SolvedSudoku {

  def normalizeCanvas(normalized: Mat
                      , cellValues: Seq[Int]
                      , cellRois: Seq[Rect]
                      , digitLibrary: DigitLibrary): Mat = {

    (cellValues zip cellRois).foldLeft(normalized) {
      case (norm, (s, r)) =>
        val cell =
          digitLibrary.digits(s).optMat match {
            case None => SudokuUtils.mkFallback(s, digitLibrary).get
            case Some(s) => s
          }
        copyTo(norm, cell, r)
    }
    /*
        for ((s, r) <- cellValues zip cellRois if s != 0) {
          val cell: Mat = {
            digitLibrary(s)._2 match {
              case None => SudokuUtils.mkFallback(s, digitLibrary).get
              case Some(s) => s
            }
          }
          copyTo(normalized, cell, r)
        }
        // will be mutated during for loop
        normalized*/
  }

  def apply(frameNr: Int
            , inputFrame: Mat
            , normalized: Mat
            , detectedCorners: Mat
            , cellRois: Seq[Rect]
            , solvedSudoku: SudokuHistory
            , digitLibrary: DigitLibrary): SolvedSudoku = {
    val cNormalized: Mat = normalizeCanvas(normalized, solvedSudoku.cellValues, cellRois, digitLibrary)
    JavaCV.writeMat(Sudoku.targetPath.resolve(s"$frameNr-solvedCanvasNormalized.png"), cNormalized)

    val warped: Mat = JavaCV.unwarp(cNormalized, detectedCorners)
    JavaCV.writeMat(Sudoku.targetPath.resolve(s"$frameNr-warped.png"), warped)
    val solutionMat: Mat = JavaCV.copySrcToDestWithMask(warped, inputFrame, warped)
    JavaCV.writeMat(Sudoku.targetPath.resolve(s"$frameNr-solution.png"), solutionMat)

    SolvedSudoku(solutionMat
      , Option(cNormalized)
      , detectedCorners
      , solvedSudoku)
  }

}

case class SolvedSudoku(frameWithSolution: Mat
                        , optCNormalized: Option[Mat]
                        , detectedCorners: Mat
                        , sudokuHistory: SudokuHistory)
