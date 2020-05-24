package net.ladstatt.opencv

import java.io.File

import net.ladstatt.core.CanLog
import net.ladstatt.sudoku._
import org.bytedeco.javacpp.{BytePointer, DoublePointer}
import org.bytedeco.opencv.global._
import org.bytedeco.opencv.global.opencv_core._
import org.bytedeco.opencv.global.opencv_imgcodecs._
import org.bytedeco.opencv.global.opencv_imgproc._
import org.bytedeco.opencv.opencv_core.{Mat, Point, Point2f, Scalar, Size, _}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/**
 * Contains functions which are backed by JavaCV's API to OpenCV.
 */
object JavaCV extends CanLog {

  import Parameters._

  lazy val EmptyCorners = new Point2f

  lazy val Kernel: Mat = {
    opencv_imgproc.getStructuringElement(MORPH_RECT, new Size(3, 3))
  }


  def copyMat(orig: Mat): Mat = {
    val dest = new Mat()
    orig.copyTo(dest)
    dest
  }

  def copyTo(data: Mat, canvas: Mat, roi: Rect): Unit = {
    val cellTarget = new Mat(canvas, roi)
    data.copyTo(cellTarget)
  }

  def paintRect(canvas: Mat, rect: Rect, color: Scalar, thickness: Int): Unit = {
    opencv_imgproc.rectangle(canvas, rect.tl(), rect.br(), color, thickness, LINE_8, 0)
  }

  /**
   * Given a list of curves, this function returns the curve with the biggest area which is described by the
   * contour.
   *
   * @param curveList
   * @return
   */
  def extractCurveWithMaxArea(curveList: MatVector): (Double, Mat) = {
    if (curveList.size == 0) {
      (0.0, new Mat)
    } else {
      curveList.get().foldLeft[(Double, Mat)]((0.0, new Mat)) {
        case ((area, c), curve) =>
          val cArea = opencv_imgproc.contourArea(curve)
          if (cArea >= area) (cArea, curve) else (area, c)
      }
    }
  }


  /**
   * (a,b,c,d)
   *
   * @param corners
   * @return
   */
  def isSomewhatSquare(corners: Seq[Point]): Boolean = {

    import scala.math.{abs, atan2}

    def calcAngle(a: Point, b: Point) = {
      atan2((b.y - a.y).toDouble
        , (b.x - a.x).toDouble) * 180 / scala.math.Pi
    }

    if (corners.nonEmpty) {
      val hasAlignedAngles: Boolean =
        abs(calcAngle(corners(0), corners(1)) - calcAngle(corners(3), corners(2))) < 10 &&
          abs(calcAngle(corners(0), corners(3)) - calcAngle(corners(1), corners(2))) < 10

      hasAlignedAngles
    } else {
      false
    }
  }

  def mkCorners(size: Size): Mat = {
    val (width, height) = (size.width, size.height)
    val m = new Mat()
    m.push_back_(new Point(0, 0))
    m.push_back_(new Point(width, 0))
    m.push_back_(new Point(width, height))
    m.push_back_(new Point(0, height))
    m
  }


  def toMat(buffer: Array[Byte], size: Size): Mat = {
    val x = size
    println(x)
    val m = new Mat(new BytePointer(buffer: _*))
    m
  }

  def persist(mat: Mat, file: File): Try[File] =
    Try {
      logWithTimer(s"Wrote ${file.getAbsolutePath}", {
        if (!imwrite(file.getAbsolutePath, mat)) {
          throw new RuntimeException(s"Could not save to file $file")
        } else {
          file
        }
      })
    }

  /*
    def alphaBlend(src: Mat, alpha: Mat): Mat = {
      val channels = new java.util.ArrayList[Mat]()
      opencv_core.split(src, channels)
      channels.add(alpha)
      val merged = new Mat
      opencv_core.merge(channels, merged)
      merged
    }
  */
  /*
  def mkMatWithCurve(image: Mat, curve: MatOfPoint2f, color: Scalar, thickness: Int): Future[Mat] = {
    mkMatWithCurve(image, curve.toList.asScala.toList, color, thickness)
  }

  def mkMatWithCurve(image: Mat, points: List[Point], color: Scalar, thickness: Int): Future[Mat] =
    Future {
      if (points.size > 2) {
        for (linePoints <- points.sliding(2)) {
          opencv_imgproc.line(image, linePoints(0), linePoints(1), color, thickness)
        }
      }
      image
    }*/

  /**
   * wraps equalizeHist from Imgproc
   *
   * @param input
   * @return
   */
  def equalizeHist(input: Mat): Future[Mat] =
    Future {
      val output = new Mat
      opencv_imgproc.equalizeHist(input, output)
      output
    }


  def norm(mat: Mat): Future[Mat] = {
    for {
      b <- FramePipeline.gaussianblur(mat)
      dilated <- FramePipeline.dilate(b, JavaCV.Kernel)
      thresholded <- FramePipeline.adaptiveThreshold(dilated, 255, 9)
    } yield thresholded
  }

  /**
   * Returns position and value for a template for a given image
   *
   * @return
   */
  def matchTemplate(candidate: Mat, number: Int, withNeedle: Mat): Future[(Int, Double)] = {

    val normedCandidateF = norm(candidate)
    val normedNeedleF = norm(withNeedle)

    val result =
      for {
        c <- normedCandidateF
        needle <- normedNeedleF
      }
        yield {
          val width = candidate.cols - withNeedle.cols + 1
          val height = candidate.rows - withNeedle.rows + 1
          val resultImage = new Mat(width, height, CV_32FC1)
          opencv_imgproc.matchTemplate(c, needle, resultImage, opencv_imgproc.TM_SQDIFF)
          val minValPointer = new DoublePointer()
          opencv_core.minMaxLoc(resultImage, minValPointer)
          //        OpenCV.persist(c, new File(s"target/${number}_${minMaxResult.minVal}_candidate_.png"))
          //        OpenCV.persist(needle, new File(s"target/${number}_${minMaxResult.minVal}_needle_.png"))
          (number, minValPointer.get())
        }
    result
  }

  def resize(s: Mat, size: Size): Mat = {
    val dest = new Mat()
    opencv_imgproc.resize(s, dest, size)
    dest
  }

  def resizeFuture(source: Mat, size: Size): Future[Mat] = Future(resize(source, size))


  /**
   * warps image from to make feature extraction's life easier (time intensive call)
   */
  def warp(input: Mat, srcCorners: Mat, destCorners: Mat): Mat = {
    // require(srcCorners.toList.size == 4)
    // require(destCorners.toList.size == 4)

    val transformationMatrix: Mat = getPerspectiveTransform(srcCorners, destCorners)

    val dest = new Mat()
    warpPerspective(input, dest, transformationMatrix, input.size())
    dest
  }

  // input mat will be altered by the findContours(...) function
  def findContours(original: Mat, mode: Int, method: Int): MatVector = {
    val input: Mat = copyMat(original)
    val contours = new MatVector()
    opencv_imgproc.findContours(input, contours, new Mat, mode, method)
    contours
  }

  def has4Sides(needle: Mat): Boolean = needle.size == new Size(1, 4)

  def mkApproximation(curve: Mat, epsilon: Double = 0.02): Mat = {
    val arcLength = opencv_imgproc.arcLength(curve, true)
    val approxCurve = new Mat
    opencv_imgproc.approxPolyDP(curve, approxCurve, epsilon * arcLength, true)
    approxCurve
  }

  def contourPredicate(sp: SpecialParam)(c: Mat): Boolean = {
    val boundingRect = opencv_imgproc.boundingRect(c)
    val area = boundingRect.area
    (sp.minArea < area) && (area < sp.maxArea) && boundingRect.contains(sp.center)
  }


  /**
   * Given a list of contours and a predicate function this function returns the "best fit"
   * bounding rectangle like defined in the predicate function.
   *
   * @param contours  the list of contours
   * @param predicate the predicate function
   * @return
   */
  def findBestFit(contours: MatVector, predicate: Mat => Boolean): Option[Rect] = {
    contours.get().foldLeft[Option[(Double, Mat)]](None) {
      case (acc, c) =>
        if (predicate(c)) {
          val area = opencv_imgproc.contourArea(c)
          acc match {
            case None => Some((area, c))
            case Some((a, _)) =>
              if (area > a) Some((area, c)) else acc
          }
        } else acc
    } map {
      case (_, contour) => opencv_imgproc.boundingRect(contour)
    }

  }


  def findCellContour(original: Mat,
                      specialParam: SpecialParam,
                      contourMode: Int,
                      contourMethod: Int): Option[Mat] = {
    val contours: MatVector = findContours(original, contourMode, contourMethod)
    val predicateFn: Mat => Boolean = contourPredicate(specialParam)
    val maybeRect: Option[Rect] = findBestFit(contours, predicateFn)
    maybeRect.map(r => original.apply(r))
  }

  /**
   * sort points in following order:
   * topleft, topright, bottomright, bottomleft
   */
  /*
def mkSortedCorners(points: MatOfPoint2f): Seq[Point] = {
  val pointsAsList = points.toList
  val sortBySum = pointsAsList.asScala.sortWith((l, r) => (l.x + l.y) < (r.x + r.y))
  val sortByDifference = pointsAsList.asScala.sortWith((l, r) => (l.y - l.x) < (r.y - r.x))
  val (topleft, bottomright) = (sortBySum.head, sortBySum.reverse.head)
  val (topright, bottomleft) = (sortByDifference.head, sortByDifference.reverse.head)
  Seq(topleft, topright, bottomright, bottomleft)
}
*/

  def threshold(input: Mat): Future[Mat] =
  Future {
    val output = new Mat
    opencv_imgproc.threshold(input, output, 30, 255, opencv_imgproc.THRESH_BINARY)
    output
  }


  /*
    def blur(input: Mat): Future[Mat] =
      Future {
        val dest = new Mat()
        opencv_imgproc.blur(input, dest, new Size(20, 20), new Point(-1, -1))
        dest
      } */


  def filter2D(kernel: Mat)(input: Mat): Mat = {
    val out = new Mat
    opencv_imgproc.filter2D(input, out, -1, kernel)
    out
  }


  case class SpecialParam(center: Point, minArea: Double, maxArea: Double)

  // only search for contours in a subrange of the original cell to get rid of possible border lines
  // TODO: remove, likely not really necessary
  def specialize(cellRawData: Mat): Future[(Mat, SpecialParam)] =
    Future {
      val (width, height) = (cellRawData.size.width, cellRawData.size.height)
      val cellData = new Mat(cellRawData, new Range((height * 0.1).toInt, (height * 0.9).toInt), new Range((width * 0.1).toInt, (width * 0.9).toInt))
      val cellArea = cellData.size().area
      val (minArea, maxArea) = (0.15 * cellArea, 0.5 * cellArea)
      val (centerX, centerY) = (cellData.size.width / 2, cellData.size.height / 2)
      (cellData, SpecialParam(new Point(centerX, centerY), minArea, maxArea))
    }


  // filter out false positives
  // use information known (size, position of digits)
  // the bounding box of the contours must fit into some rough predicate, like follows:
  // the area must be of a certain size
  // the area must not be greater than a certain size
  // the center of the image has to be part of the bounding rectangle
  def extractContour(coloredCell: Mat): Future[Option[Mat]] = {
    for {
      cell <- FramePipeline.toGray(coloredCell)
      (cellData, sp) <- specialize(cell)
      equalized <- equalizeHist(cellData)
      blurred <- FramePipeline.gaussianblur(equalized)
      thresholded <- threshold(blurred)
      a <- FramePipeline.bitwiseNot(thresholded)
    } yield {
      val someMat = findCellContour(a, sp, opencv_imgproc.RETR_TREE, opencv_imgproc.CHAIN_APPROX_SIMPLE)

      //   someMat foreach (Imgcodecs.imwrite(new File("/Users/lad/Documents/sudokufx/sudoku-core/target/" + UUID.randomUUID().toString + ".png").getAbsolutePath, _))
      someMat
    }
  }

  def mkCellSize(sudokuSize: Size): Size = new Size(sudokuSize.width / ssize, sudokuSize.height / ssize)

  def mkRect(i: Int, width: Int, height: Int): Rect = new Rect(Parameters.col(i) * width, Parameters.row(i) * height, width, height)

  def mkRect(i: Int, s: Size): Rect = new Rect(Parameters.col(i) * s.width, Parameters.row(i) * s.height, s.width, s.height)


}