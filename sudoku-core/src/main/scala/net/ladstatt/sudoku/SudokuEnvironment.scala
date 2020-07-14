package net.ladstatt.sudoku

import java.nio.file.Path

import net.ladstatt.core.CanLog
import net.ladstatt.sudoku.JavaCV.{extractCurveWithMaxArea, has4Sides}
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{Mat, MatVector}

/**
 * In this class all data for a sudoku is saved an passed through the algorithmic steps.
 */
object SudokuEnvironment {

  /**
   * provide path to frame image and expected solution
   *
   * @param path path to frame
   * @return
   */
  def apply(id: String
            , frameNr: Int
            , path: Path
            , corners: Seq[Float]
            , history: SudokuHistory
            , library: DigitLibrary): SudokuEnvironment = {
    new SudokuEnvironment(id
      , frameNr
      , JavaCV.loadMat(path)
      , corners
      , ContourParams()
      , history
      , library)
  }

}

case class SudokuEnvironment(id: String
                             , frameNr: Int
                             , frame: Mat
                             , corners: Seq[Float]
                             , contourParams: ContourParams
                             , history: SudokuHistory
                             , library: DigitLibrary
                             , someSolutionMat: Option[Mat] = None
                             , resultCells: Seq[SCell] = Seq()
                            ) extends CanLog {


  val grayed: Mat = JavaCV.toGray(frame)
  val blurred: Mat = JavaCV.gaussianblur(grayed)
  val thresholded: Mat = JavaCV.adaptiveThreshold(blurred)
  val inverted: Mat = JavaCV.bitwiseNot(thresholded)
  val dilated: Mat = JavaCV.dilate(inverted)
  //

  lazy val someRectangle: Option[Mat] = {
    val inputFrameCorners = JavaCV.mkCorners(frame)
    val contours: MatVector = JavaCV.findContours(dilated, contourParams.retrivalMode, contourParams.approximation)
    val minimumExpectedArea: Double = opencv_imgproc.contourArea(inputFrameCorners) / contourParams.contourRatio
    val maybeMat = detectRectangle(contours, minimumExpectedArea)
    maybeMat
  }


  /** frames without rectangles get filtered out to a None, otherwise start processing Sudoku */
  lazy val optSudoku: Option[Sudoku] = {
    someRectangle.map(detectedCorners => {
      Sudoku(id, frameNr, frame, detectedCorners, history, library)
    })
  }


  /**
   * Awaits a preprocessed video frame and finds the corners of the biggest rectangle seen
   * in the input.
   *
   * @return detected contours
   */
  private def detectRectangle(contours: MatVector, minimumExpectedArea: Double): Option[Mat] = {
    val (contourArea, curve) = extractCurveWithMaxArea(contours)
    if (contourArea > minimumExpectedArea) {
      // logTrace("Found suitable area")
      Option(JavaCV.approximate(curve)).filter(has4Sides).map(m => {
        // logTrace("found 4 sides ... ")
        JavaCV.sortCorners(m)
      })
    } else {
      // logTrace(s"The detected area of interest was too small ($contourArea < $minimumExpectedArea).")
      None
    }
  }


}