package net.ladstatt.sudoku

import java.io.File

import net.ladstatt.opencv.JavaCV
import org.bytedeco.javacv.{Frame, OpenCVFrameConverter}
import org.bytedeco.opencv.global.opencv_core.BORDER_CONSTANT
import org.bytedeco.opencv.global.opencv_imgproc.morphologyDefaultBorderValue
import org.bytedeco.opencv.global.{opencv_core, opencv_imgcodecs, opencv_imgproc}
import org.bytedeco.opencv.opencv_core.{Mat, MatVector, Point, Scalar, Size}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object FramePipeline {

  val javaCVConv = new OpenCVFrameConverter.ToMat
  def apply(frame: Frame, params: SParams ): FramePipeline = {
    apply(javaCVConv.convert(frame), params)
  }

  def gaussianblur(input: Mat): Future[Mat] =
    Future {
      val dest = new Mat()
      opencv_imgproc.GaussianBlur(input, dest, new Size(11, 11), 0)
      dest
    }

  def adaptiveThreshold(input: Mat,
                        maxValue: Double = 255,
                        blockSize: Int = 5,
                        c: Double = 2,
                        adaptiveMethod: Int = opencv_imgproc.ADAPTIVE_THRESH_MEAN_C): Future[Mat] =
    Future {
      val thresholded = new Mat()
      opencv_imgproc.adaptiveThreshold(input, thresholded, maxValue, adaptiveMethod, opencv_imgproc.THRESH_BINARY, blockSize, c)
      thresholded
    }

  def bitwiseNot(input: Mat): Future[Mat] =
    Future {
      val output = new Mat
      opencv_core.bitwise_not(input, output)
      output
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
  def toGray(input: Mat): Future[Mat] =
    Future {
      val grayed = new Mat
      opencv_imgproc.cvtColor(input, grayed, opencv_imgproc.COLOR_BGR2GRAY)
      grayed
    }

  def erode(input: Mat): Future[Mat] =
    Future {
      val output = new Mat
      val m = opencv_imgproc.getStructuringElement(opencv_imgproc.MORPH_CROSS,
        new Size(1, 1),
        new Point(0, 0))
      opencv_imgproc.erode(input, output, m)
      output
    }

  def dilate(input: Mat, kernel: Mat): Future[Mat] =
    Future {
      val output = new Mat
      val anchor = new Point(-1, -1)
      opencv_imgproc.dilate(input, output, kernel, anchor, 2,BORDER_CONSTANT,new Scalar(morphologyDefaultBorderValue()))
      output
    }

  def apply(frame: Mat, params: SParams): FramePipeline = {
    val start = System.nanoTime()
    Await.result(for {
      working <- Future(copySrcToDestWithMask(frame, new Mat, frame))
      grayed <- toGray(working)
      blurred <- gaussianblur(grayed)
      thresholdApplied <- adaptiveThreshold(blurred)
      inverted <- bitwiseNot(thresholdApplied)
      dilated <- dilate(inverted, JavaCV.Kernel)
      eroded <- erode(inverted)
    } yield FramePipeline(start
      , frame
      , working
      , grayed
      , blurred
      , thresholdApplied
      , inverted
      , dilated
      , eroded
      , params), Duration.Inf)
  }

}


/**
 * the result for one frame. a frame is a image from the image stream
 */
case class FramePipeline(start: Long
                         , frame: Mat
                         , working: Mat
                         , grayed: Mat
                         , blurred: Mat
                         , thresholded: Mat
                         , inverted: Mat
                         , dilated: Mat
                         , eroded: Mat
                         , params: SParams) extends SResult {

  val corners = JavaCV.mkCorners(frame.size)
  /**
   * a sequence of point lists which give the recognized contour lines
   */
  lazy val contours: MatVector = JavaCV.findContours(dilated, params.contourMode, params.contourMethod)

  /**
   * returns coordinates of detected sudoku
   */
  lazy val detectRectangle: Option[Mat] = SudokuUtils.detectRectangle(corners, params, contours)

  def persist(dir: File): Unit = {
    dir.mkdirs()
    opencv_imgcodecs.imwrite(new File(dir, "frame.png").getAbsolutePath, frame)
    opencv_imgcodecs.imwrite(new File(dir, "working.png").getAbsolutePath, working)
    opencv_imgcodecs.imwrite(new File(dir, "grayed.png").getAbsolutePath, grayed)
    opencv_imgcodecs.imwrite(new File(dir, "blurred.png").getAbsolutePath, blurred)
    opencv_imgcodecs.imwrite(new File(dir, "thresholded.png").getAbsolutePath, thresholded)
    opencv_imgcodecs.imwrite(new File(dir, "inverted.png").getAbsolutePath, inverted)
    opencv_imgcodecs.imwrite(new File(dir, "dilated.png").getAbsolutePath, dilated)
    opencv_imgcodecs.imwrite(new File(dir, "eroded.png").getAbsolutePath, eroded)
    ()
  }


}





