package net.ladstatt.sudoku

import java.nio.FloatBuffer
import java.nio.file.Path

import net.ladstatt.core.CanLog
import net.ladstatt.sudoku.JavaCV.{extractCurveWithMaxArea, has4Sides}
import net.ladstatt.sudoku.Sudoku.targetPath
import org.bytedeco.javacpp.FloatPointer
import org.bytedeco.opencv.global.{opencv_core, opencv_imgproc}
import org.bytedeco.opencv.opencv_core.{Mat, MatVector, Size}

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
                             , frameNr: Int
                             , frame: Mat
                             , corners: Seq[Float]
                             , contourParams: ContourParams
                             , history: SudokuHistory
                             , library: DigitLibrary = Parameters.defaultDigitLibrary
                             , someSolutionMat: Option[Mat] = None
                             , resultCells: Seq[SCell] = Seq()
                            ) extends CanLog {


  val grayed: Mat = JavaCV.toGray(frame)
  val blurred: Mat = JavaCV.gaussianblur(grayed)
  val thresholded: Mat = JavaCV.adaptiveThreshold(blurred)
  val inverted: Mat = JavaCV.bitwiseNot(thresholded)
  val dilated: Mat = JavaCV.dilate(inverted)
  //


  /**
   * This mat contains an 'unstretched' version of the detected sudoku outer rectangle.
   *
   * In this representation it is easier to paint upon. After painting this Mat will be retransformed
   * to the original appearance again.
   */


  /** frames without rectangles get filtered out to a None, otherwise start processing Sudoku */
  lazy val optSudoku: Option[Sudoku] = {
    val inputFrameCorners = JavaCV.mkCorners(frame)
    val contours: MatVector = JavaCV.findContours(dilated, contourParams.retrivalMode, contourParams.approximation)
    val minimumExpectedArea: Double = opencv_imgproc.contourArea(inputFrameCorners) / contourParams.contourRatio
    detectRectangle(contours, minimumExpectedArea).map(detectedCorners => {
      val normalized = JavaCV.warp(frame, detectedCorners)
      if ((frameNr % 25) == 0) {
        JavaCV.writeMat(targetPath.resolve(frameNr + "-frame.png"), frame)
        JavaCV.writeMat(targetPath.resolve(frameNr + "-normalized.png"), normalized)
        JavaCV.writeMat(Sudoku.targetPath.resolve(frameNr + "-dilated.png"), dilated)
      }
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
      //logTrace("corners: " + corners)
      Sudoku(id, frameNr, frame, normalized, corners, detectedCorners, history, library)
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