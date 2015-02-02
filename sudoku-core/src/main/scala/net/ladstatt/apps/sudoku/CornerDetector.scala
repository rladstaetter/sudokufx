package net.ladstatt.apps.sudoku

import net.ladstatt.opencv.OpenCV
import org.opencv.core.{Mat, MatOfPoint2f}

/**
 * Created by lad on 02.02.15.
 */

object CornerDetector {

  val EmptyCorners = new MatOfPoint2f
}

case class CornerDetector(dilated: Mat) {

  val corners: MatOfPoint2f = OpenCV.detectSudokuCorners(dilated)

  val foundCorners: Boolean = {
    !corners.empty
  }

}
