package net.ladstatt.sudoku

import java.nio.file.{Path, Paths}

import net.ladstatt.core.CanLog
import net.ladstatt.sudoku.JavaCV.{extractCurveWithMaxArea, has4Sides}
import org.bytedeco.javacpp.FloatPointer
import org.bytedeco.opencv.global.{opencv_core, opencv_imgproc}
import org.bytedeco.opencv.opencv_core.{Mat, MatVector, Size}

import scala.concurrent.duration.FiniteDuration

/**
 * In this class all data for a sudoku is saved an passed through the algorithmic steps.
 */
object SudokuEnvironment {

  /**
   * provide path to frame image and expected solution
   *
   * @param path                path to frame
   * @return
   */
  def apply(id: String
            , index: Int
            , path: Path
            , corners: Seq[Float]
            , hitCounters: SudokuHistory): SudokuEnvironment = {
    new SudokuEnvironment(id
      , index
      , JavaCV.loadMat(path)
      , corners
      , ContourParams()
      , hitCounters)
  }
}

case class SudokuEnvironment(id: String
                             , index: Int
                             , frame: Mat
                             , corners: Seq[Float]
                             , contourParams: ContourParams
                             , history: SudokuHistory
                             , library: DigitLibrary = Parameters.defaultDigitLibrary
                             , cap: Int = Parameters.cap
                             , minHits: Int = Parameters.minHits
                             , maxSolvingDuration: FiniteDuration = Parameters.maxSolvingDuration
                             , someSolutionMat: Option[Mat] = None
                             , resultCells: Seq[SCell] = Seq()
                            ) extends CanLog {

  lazy val working: Mat = JavaCV.copySrcToDestWithMask(frame)
  lazy val grayed: Mat = JavaCV.toGray(working)
  lazy val blurred: Mat = JavaCV.gaussianblur(grayed)
  lazy val thresholded: Mat = JavaCV.adaptiveThreshold(blurred)
  lazy val inverted: Mat = JavaCV.bitwiseNot(thresholded)
  lazy val dilated: Mat = JavaCV.dilate(inverted)
  lazy val eroded: Mat = JavaCV.erode(inverted)

  lazy val optSudoku: Option[Sudoku] = {
    val contours: MatVector = JavaCV.findContours(dilated, contourParams.retrivalMode, contourParams.approximation)
    val inputFrameCorners = JavaCV.mkCorners(frame)
    val minimumExpectedArea: Double = opencv_imgproc.contourArea(inputFrameCorners) / contourParams.contourRatio
    detectRectangle(contours, minimumExpectedArea).map(detectedCorners => {
      Sudoku(id, index, frame, detectedCorners, history, library, cap)
    })
  }


/*
  lazy val solved: SudokuEnvironment = {
    optSudoku match {
      case None =>
        // no canvas found, returns current sudoku 'state'
        logWarn("Found no sudoku on this frame.")
        this
      case Some(sudoku) =>
        sudoku.trySolve match {
          case Some(SolvedSudoku(video, sudokuConfiguration, detectedCorners, hitHistory, library, cells)) =>

          case None =>
        }
        logTrace("Found sudoku canvas.")
        val copiedSudoku = copy(
          library = sCanvas.updatedLibrary,
          sudokuHitHistory = sCanvas.updatedHits)
        val solvedSudoku = copiedSudoku.performSolving()
        if (solvedSudoku.isSolved) {
          solvedSudoku.optSudoku match {
            case None =>
              logInfo("Could not find someSudokuCanvas (??)")
              solvedSudoku
            case Some(solvedSudokuCanvas) =>
              logInfo("Found solution for sudoku:")
              logInfo(solvedSudoku.sudokuConfiguration.solutionAsString)

              val solvedCanvasNormalized: Mat = solvedSudokuCanvas.ssolvedCanvasNormalized()
              JavaCV.writeMat(Paths.get(s"target/$id-$index-solvedCanvasNormalized.png"), solvedCanvasNormalized)
              val annotatedSolution: Mat = solvedSudokuCanvas.annotated
              JavaCV.writeMat(Paths.get(s"target/$id-$index-annotated.png"), annotatedSolution)

              val warped: Mat = JavaCV.unwarp(annotatedSolution, solvedSudokuCanvas.detectedCorners)
              JavaCV.writeMat(Paths.get(s"target/$id-$index-warped.png"), warped)
              val solutionMat: Mat = JavaCV.copySrcToDestWithMask(warped, solvedSudokuCanvas.inputFrame, warped)

              solvedSudoku.copy(someSolutionMat = Option(solutionMat))
          }
        } else {

          logWarn("Could not solve sudoku.")
          copy(someSolutionMat = Option(frame))
        }
    }
  } */


  /**
   * given a frequency table, returns a number which exceed a certain threshold
   */
  def filterHits(freqs: Map[Int, Int], threshold: Int): Option[(Int, Int)] = {
    freqs.find {
      case (value, f) => value != 0 && f >= threshold
    }
  }


  /**
   * Awaits a preprocessed video frame and finds the corners of the biggest rectangle seen
   * in the input.
   *
   * @return detected contours
   */
  def detectRectangle(contours: MatVector, minimumExpectedArea: Double): Option[Mat] = {
    val (contourArea, curve) = extractCurveWithMaxArea(contours)
    if (contourArea > minimumExpectedArea) {
      // logTrace("Found suitable area")
      Option(JavaCV.approximate(curve)).filter(has4Sides).map(m => {
        // logTrace("found 4 sides ... ")
        sortCorners(m)
      })
    } else {
      // logTrace(s"The detected area of interest was too small ($contourArea < $minimumExpectedArea).")
      None
    }
  }

  /**
   * goal is to reorganize inputs in a way that all 4 coordinate pairs are returned in following ordering:
   *
   * <ul>
   * <li>left upper corner</li>
   * <li>right uper corner</li>
   * <li>right lower corner</li>
   * <li>left lower corner</li>
   * </ul>
   *
   * @param m
   * @return
   */
  def sortCorners(m: Mat): Mat = {
    val ps = JavaCV.extractIntPoints(m)

    // idea: the top left corner has the smallest, the bottom right corner the biggest distance to (0,0).
    // hence we have two points. for the other two points we just say that B has a smaller y coordinate
    // than point D. Should work for most of the cases
    val sortByDist = ps.sortWith((a, b) => a.dist < b.dist)
    val A = sortByDist.head
    val C = sortByDist.reverse.head
    val rest = ps.toSet -- Set(A, C)
    val sortByY = rest.toSeq.sortWith((a, b) => a.y < b.y)
    val B = sortByY.head
    val D = sortByY.tail.head

    val fp = new FloatPointer(A.x, A.y
      , B.x, B.y
      , C.x, C.y
      , D.x, D.y
    )
    new Mat(new Size(2, 4), opencv_core.CV_32F, fp)
  }

}