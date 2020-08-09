package net.ladstatt.sudoku

import java.nio.FloatBuffer
import java.nio.file.Path

import net.ladstatt.sudoku.JavaCV._
import org.bytedeco.opencv.opencv_core.{Mat, Rect}

import scala.concurrent.duration._
import scala.language.postfixOps

object Sudoku {

  /**
   * the maximum time the algorithm should search for a solution
   */
  val maxSolvingDuration: FiniteDuration = 1500 millis

  /** if true write debug info for cell */
  val writeCellDebug: Boolean = false

  /** don't try to solve sudoku until a certain amount of cells containing numbers are detected */
  val minNrOfDetectedCells = 20

  /**
   * for a given cell, detect at least minNrOfValueHits times a number 'to be sure' that it is really
   * the number we are after
   **/
  val minNrOfValueHits = 20

  /* a hit has to reach this quality in order to be used as a value (lower is better)*/
  //val minQuality = 25000000
  //val minQuality = 20000000
  val minQuality = 15500000
  // val minQuality = 12500000
  //val minQuality = 3500000

  def apply(persistData: Boolean
            , id: String
            , frameNr: Int
            , frame: Mat
            , detectedCorners: Mat
            , history: SudokuState
            , library: DigitLibrary
            , sessionPath: Path): Sudoku = {
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
      case (r, i) => SCell(persistData, id, frameNr, i, normalized.apply(r), r, history.hitHistory(i), sessionPath)
    }
    val updatedLibrary = library.add(normalized, cells.filter(_.detectedValue != 0))
    val updatedHistory = history.add(SudokuState(cells.map(_.detectedValue), cells.map(_.hits)))

    Sudoku(persistData
      , id
      , frameNr
      , frame
      , normalized
      , corners
      , detectedCorners
      , cellRois
      , cells
      , updatedHistory
      , updatedLibrary
      , sessionPath)
  }
}

/**
 * Created by lad on 01.05.16.
 *
 * @param frame           video input
 * @param detectedCorners corner points of detected sudoku
 */
case class Sudoku(persistData: Boolean
                  , id: String
                  , frameNr: Int
                  , frame: Mat
                  , normalized: Mat
                  , corners: Seq[Float]
                  , detectedCorners: Mat
                  , cellRois: Seq[Rect]
                  , cells: Seq[SCell]
                  , history: SudokuState
                  , digitLibrary: DigitLibrary
                  , sessionPath: Path) {

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
      val solvedSudoku: SudokuState = history.solved
      if (solvedSudoku.isSolved) {
        Option(SolvedSudoku(persistData,
          frameNr
          , frame
          , normalized
          , detectedCorners
          , cellRois
          , history.solved
          , digitLibrary
          , sessionPath))
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
      Option(SolvedSudoku(
        frameNr
        , frame
        , None
        , detectedCorners
        , history))
    }
  }


}




