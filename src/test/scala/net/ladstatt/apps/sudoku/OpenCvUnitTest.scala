package net.ladstatt.apps.sudoku

import net.ladstatt.opencv.OpenCV
import net.ladstatt.opencv.OpenCV._
import org.opencv.core.{CvType, Mat}
import org.opencv.highgui.Highgui

/**
 * Created by lad on 26.10.14.
 */
trait OpenCvUnitTest {

  loadNativeLib()

  lazy val frame69: Mat = Highgui.imread("src/test/resources/frame69.png")
  lazy val emptyFrame = new Mat(1280, 768, CvType.CV_8UC3)
}
