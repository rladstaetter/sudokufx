package net.ladstatt.sudoku

import org.opencv.core.Mat

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object ImageIOChain {

  import net.ladstatt.opencv.OpenCV._

  def apply(frame: Mat): ImageIOChain = {
    Await.result(for {
      working <- copySrcToDestWithMask(frame, new Mat, frame)
      grayed <- toGray(working)
      blurred <- gaussianblur(grayed)
      thresholdApplied <- adaptiveThreshold(blurred)
      inverted <- bitwiseNot(thresholdApplied)
      dilated <- dilate(inverted)
      eroded <- erode(inverted)
    //  dilated <- dilate(thresholdApplied)
    //  inverted <- bitwiseNot(dilated)
    } yield ImageIOChain(working, grayed, blurred, thresholdApplied, inverted, dilated, eroded), Duration.Inf)
  }

}

/**
  * the result for one frame. a frame is a image from the image stream
  */
case class ImageIOChain(working: Mat, grayed: Mat,
                        blurred: Mat, thresholded: Mat,
                        inverted: Mat, dilated: Mat, eroded: Mat)
