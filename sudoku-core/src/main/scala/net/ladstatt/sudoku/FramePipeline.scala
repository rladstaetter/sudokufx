package net.ladstatt.sudoku

import java.io.File

import net.ladstatt.opencv.OpenCV
import org.opencv.core.Mat
import org.opencv.imgcodecs.Imgcodecs

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object FramePipeline {

  import net.ladstatt.opencv.OpenCV._

  def apply(frame: Mat): FramePipeline = {
    val start = System.nanoTime()
    Await.result(for {
      working <- copySrcToDestWithMask(frame, new Mat, frame)
      grayed <- toGray(working)
      blurred <- gaussianblur(grayed)
      thresholdApplied <- adaptiveThreshold(blurred)
      inverted <- bitwiseNot(thresholdApplied)
      dilated <- dilate(inverted, OpenCV.Kernel)
      eroded <- erode(inverted)
    //  dilated <- dilate(thresholdApplied)
    //  inverted <- bitwiseNot(dilated)
    } yield FramePipeline(start, frame, working, grayed, blurred, thresholdApplied, inverted, dilated, eroded), Duration.Inf)
  }

}

/**
  * the result for one frame. a frame is a image from the image stream
  */
case class FramePipeline(start: Long,
                         frame: Mat, working: Mat, grayed: Mat,
                         blurred: Mat, thresholded: Mat,
                         inverted: Mat, dilated: Mat, eroded: Mat) {

  def persist(dir: File): Unit = {
    dir.mkdirs()
    Imgcodecs.imwrite(new File(dir, "frame.png").getAbsolutePath, frame)
    Imgcodecs.imwrite(new File(dir, "dilated.png").getAbsolutePath, dilated)
    Imgcodecs.imwrite(new File(dir, "eroded.png").getAbsolutePath, eroded)
  }


}
