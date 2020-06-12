package net.ladstatt.sudoku

import java.nio.file.Path

import org.bytedeco.javacv.{Frame, OpenCVFrameConverter}
import org.bytedeco.opencv.global.opencv_core.BORDER_CONSTANT
import org.bytedeco.opencv.global.opencv_imgproc.morphologyDefaultBorderValue
import org.bytedeco.opencv.global.{opencv_core, opencv_imgproc}
import org.bytedeco.opencv.opencv_core._

object FramePipeline {

  val javaCVConv = new OpenCVFrameConverter.ToMat

  def gaussianblur(input: Mat): Mat = {
    val mat = new Mat()
    opencv_imgproc.GaussianBlur(input, mat, new Size(11, 11), 0)
    mat
  }

  def adaptiveThreshold(input: Mat,
                        maxValue: Double = 255,
                        blockSize: Int = 5,
                        c: Double = 2,
                        adaptiveMethod: Int = opencv_imgproc.ADAPTIVE_THRESH_MEAN_C): Mat = {
    val mat = new Mat()
    opencv_imgproc.adaptiveThreshold(input, mat, maxValue, adaptiveMethod, opencv_imgproc.THRESH_BINARY, blockSize, c)
    mat
  }

  def bitwiseNot(input: Mat): Mat = {
    val mat = new Mat
    opencv_core.bitwise_not(input, mat)
    mat
  }

  /**
   * copies source to destination Mat with given mask and returns the destination mat.
   *
   * @param source
   * @param destination
   * @param pattern
   * @return
   */
  def copySrcToDestWithMask(source: Mat, destination: Mat, pattern: Mat): Mat = {
    source.copyTo(destination, pattern)
    destination
  }

  /**
   * converts the input mat to another color space
   *
   * @param input
   * @return
   */
  def toGray(input: Mat): Mat = {
    val mat = new Mat
    opencv_imgproc.cvtColor(input, mat, opencv_imgproc.COLOR_BGR2GRAY)
    mat
  }

  /**
   * Simplifies curve
   *
   * @param curve
   * @return
   */
  def approximate(curve: Mat): Mat = {
    val points = new Mat
    opencv_imgproc.approxPolyDP(curve
      , points
      , 0.02 * opencv_imgproc.arcLength(curve, true)
      , true)
    points
  }

  val erodeParam: Mat = opencv_imgproc.getStructuringElement(opencv_imgproc.MORPH_CROSS,
    new Size(1, 1),
    new Point(0, 0))

  def erode(input: Mat): Mat = {
    val output = new Mat
    opencv_imgproc.erode(input, output, erodeParam)
    output
  }

  val dilateAnchor = new Point(-1, -1)
  val dilateScalar = new Scalar(morphologyDefaultBorderValue())

  def dilate(input: Mat): Mat = {
    val mat = new Mat
    opencv_imgproc.dilate(input, mat, JavaCV.Kernel, dilateAnchor, 2, BORDER_CONSTANT, dilateScalar)
    mat
  }

  def copySrcToDestWithMask(source: Mat): Mat = {
    val destination = new Mat
    source.copyTo(destination, source) // use source as pattern as well
    destination
  }

  def apply(frame: Frame): FramePipeline = {
    apply(javaCVConv.convert(frame))
  }


}


/**
 * the result for one frame. a frame is a image from the image stream
 */
case class FramePipeline(frame: Mat) extends SResult {

  import FramePipeline._

  val start: Long = System.nanoTime()
  val working: Mat = copySrcToDestWithMask(frame)
  val grayed: Mat = toGray(working)
  val blurred: Mat = gaussianblur(grayed)
  val thresholded: Mat = adaptiveThreshold(blurred)
  val inverted: Mat = bitwiseNot(thresholded)
  val dilated: Mat = dilate(inverted)
  val eroded: Mat = erode(inverted)

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





