package net.ladstatt.sudoku

import java.nio.file.Path

import org.bytedeco.opencv.opencv_core._

/**
 * Groups some operations on the original video input stream together.
 *
 * @param frame one input frame to be analyzed
 */
case class FramePipeline(frame: Mat) {

  val working: Mat = JavaCV.copySrcToDestWithMask(frame)
  val grayed: Mat = JavaCV.toGray(working)
  val blurred: Mat = JavaCV.gaussianblur(grayed)
  val thresholded: Mat = JavaCV.adaptiveThreshold(blurred)
  val inverted: Mat = JavaCV.bitwiseNot(thresholded)
  val dilated: Mat = JavaCV.dilate(inverted)
  val eroded: Mat = JavaCV.erode(inverted)

  def persist(dir: Path): Unit = {
    JavaCV.writeMat(dir.resolve("frame.png"), frame)
    /*
    opencv_imgcodecs.imwrite(new File(dir, "working.png").getAbsolutePath, working)
    opencv_imgcodecs.imwrite(new File(dir, "grayed.png").getAbsolutePath, grayed)
    opencv_imgcodecs.imwrite(new File(dir, "blurred.png").getAbsolutePath, blurred)
    opencv_imgcodecs.imwrite(new File(dir, "thresholded.png").getAbsolutePath, thresholded)
    opencv_imgcodecs.imwrite(new File(dir, "inverted.png").getAbsolutePath, inverted)
    opencv_imgcodecs.imwrite(new File(dir, "dilated.png").getAbsolutePath, dilated)
    opencv_imgcodecs.imwrite(new File(dir, "eroded.png").getAbsolutePath, eroded)

     */
    ()
  }


}





