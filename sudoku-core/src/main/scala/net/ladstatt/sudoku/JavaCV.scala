package net.ladstatt.sudoku

import java.io.File
import java.nio.file.{Files, Path}
import java.nio.{ByteBuffer, DoubleBuffer, FloatBuffer, IntBuffer}

import net.ladstatt.core.CanLog
import org.bytedeco.javacpp.{BytePointer, FloatPointer}
import org.bytedeco.opencv.global._
import org.bytedeco.opencv.global.opencv_imgcodecs._
import org.bytedeco.opencv.opencv_core.{Mat, Point, Point2f, Scalar, Size, _}

/*
import org.bytedeco.javacv.*;
import org.bytedeco.javacpp.*;
import org.bytedeco.javacpp.indexer.*;
import org.bytedeco.opencv.opencv_core.*;
import org.bytedeco.opencv.opencv_imgproc.*;
import org.bytedeco.opencv.opencv_calib3d.*;
import org.bytedeco.opencv.opencv_objdetect.*;
*/
import org.bytedeco.opencv.global.opencv_core._
import org.bytedeco.opencv.global.opencv_imgproc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/**
 * Contains functions which are backed by JavaCV's API to OpenCV.
 */
object JavaCV extends CanLog {

  import Parameters._

  lazy val EmptyCorners = new Point2f


  def extractFloatPoints(m: Mat): Seq[PFloat] = {
    val bf = m.createBuffer[FloatBuffer]
    assert(bf.capacity() == 8) // we have at 4 coordinate pairs
    val a: PFloat = PFloat(bf.get(0), bf.get(1))
    val b: PFloat = PFloat(bf.get(2), bf.get(3))
    val c: PFloat = PFloat(bf.get(4), bf.get(5))
    val d: PFloat = PFloat(bf.get(6), bf.get(7))
    val ps = Seq(a, b, c, d)
    ps
  }

  def extractIntPoints(m: Mat): Seq[PInt] = {
    val bf = m.createBuffer[IntBuffer]
    assert(bf.capacity() == 8) // we have at 4 coordinate pairs
    val a: PInt = PInt(bf.get(0), bf.get(1))
    val b: PInt = PInt(bf.get(2), bf.get(3))
    val c: PInt = PInt(bf.get(4), bf.get(5))
    val d: PInt = PInt(bf.get(6), bf.get(7))
    val ps = Seq(a, b, c, d)
    ps
  }

  lazy val Kernel: Mat = {
    opencv_imgproc.getStructuringElement(MORPH_RECT, new Size(3, 3))
  }

  def mkMat(path: Path): Mat = {
    opencv_imgcodecs.imread(path.toAbsolutePath.toString)
  }

  def writeMat(target: Path, mat: Mat): Boolean = {
    Files.createDirectories(target.getParent)
    val path = target.toAbsolutePath.toString
    val r = opencv_imgcodecs.imwrite(path, mat)
    logInfo(s"Wrote $path")
    r
  }

  /**
   * Clones a Mat and creates a new one.
   *
   * @param orig original Mat
   * @return
   */
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
   * @param curveVector
   * @return
   */
  def extractCurveWithMaxArea(curveVector: MatVector): (Double, Mat) = {
    if (curveVector.size == 0) {
      (0.0, new Mat)
    } else {
      curveVector.get().foldLeft[(Double, Mat)]((0.0, new Mat)) {
        case ((area, c), curve) =>
          val cArea = opencv_imgproc.contourArea(curve)
          if (cArea >= area) (cArea, curve) else (area, c)
      }
    }
  }

  /**
   * Given a mat serving as a canvas, and a mat which contains points, draws a polygon between those points
   *
   * @param canvas
   * @param points
   * @return
   */
  def drawContours(canvas: Mat
                   , points: Mat): Mat = {
    opencv_imgproc.drawContours(canvas, new MatVector(points), -1, AbstractScalar.BLUE)
    canvas
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

  /**
   * mkCorners returns a Mat with 8 floats representing a rectangular shape with coordinates as follows:
   *
   * 0, 0
   * width, 0
   * width, height
   * 0, height
   *
   * @param size give width and height of rectangular shape
   * @return
   */
  def mkCorners2(size: Size): Mat = {
    val (width, height) = (size.width, size.height)
    val p = new Point2f(8)
    p.put(0f, 0f
      , width.toFloat, 0f
      , width.toFloat, height.toFloat
      , 0f, height.toFloat)
    new Mat(p)
  }

  def mkCorners(m: Mat): Mat = mkCorners(m.size)

  /** returns a Mat suitable for native operations (warp, perspective transform etc.)  */
  def mkCorners(size: Size): Mat = {
    mkCorners(size.width, size.height)
  }

  def mkCorners(width: Int, height: Int): Mat = {
    val p = new FloatPointer(0f, 0f
      , width.toFloat, 0f
      , width.toFloat, height.toFloat
      , 0f, height.toFloat)
    new Mat(new Size(2, 4), CV_32F, p)
  }


  def toMat(buffer: Array[Byte], size: Size): Mat = {
    val bytePointer = new BytePointer(ByteBuffer.wrap(buffer))
    val m = new Mat(size, CV_8U, bytePointer)
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
  def equalizeHist(input: Mat): Mat = {
    val output = new Mat
    opencv_imgproc.equalizeHist(input, output)
    output
  }


  def norm(mat: Mat): Future[Mat] = {
    for {
      b <- Future(FramePipeline.gaussianblur(mat))
      dilated <- Future(FramePipeline.dilate(b))
      thresholded <- Future(FramePipeline.adaptiveThreshold(dilated, 255, 9))
    } yield thresholded
  }

  /**
   * Returns position and value for a template for a given image
   *
   * @return
   */
  def matchTemplate(candidate: Mat
                    , number: Int
                    , withNeedle: Mat): Future[(Int, Double)] = {

    val normedCandidateF = norm(candidate)
    val normedNeedleF = norm(withNeedle)

    val result =
      for {
        c <- normedCandidateF
        needle <- normedNeedleF
      } yield {
        val width = candidate.cols - withNeedle.cols + 1
        val height = candidate.rows - withNeedle.rows + 1
        val resultImage = new Mat(width, height, CV_32FC1)
        opencv_imgproc.matchTemplate(c, needle, resultImage, opencv_imgproc.TM_SQDIFF)
        val dbf = DoubleBuffer.wrap(Array[Double](1))
        opencv_core.minMaxLoc(resultImage, dbf)
        (number, dbf.get(0))
      }
    result
  }

  def resize(s: Mat, size: Size): Mat = {
    val dest = new Mat()
    opencv_imgproc.resize(s, dest, size)
    dest
  }

  // def resizeFuture(source: Mat, size: Size): Future[Mat] = Future(resize(source, size))

  /*
    def warp2(imageMat: Mat, upLeft: Point, upRight: Point, downLeft: Point, downRight: Point): Mat = {
      val originalImgWidth = imageMat.size.width
      val originalImgHeight = imageMat.size.height
      val srcCorners = new FloatPointer(upLeft.x, upLeft.y, upRight.x, upRight.y, downRight.x, downRight.y, downLeft.x, downLeft.y)
      val dstCorners = new FloatPointer(0, 0, originalImgWidth.toInt, 0, originalImgWidth.toInt, originalImgHeight.toInt, 0, originalImgHeight.toInt)
      val src = new Mat(new Size(2, 4), CV_32F, srcCorners)
      val dst = new Mat(new Size(2, 4), CV_32F, dstCorners)
      val perspective = getPerspectiveTransform(src, dst)
      val result = new Mat
      warpPerspective(imageMat, result, perspective, new Size(originalImgWidth.toInt, originalImgHeight.toInt))
      src.release()
      dst.release()
      srcCorners.deallocate()
      dstCorners.deallocate()
      result
    }
  */
  def p(n: String, w: Int, h: Int) = println(s"$n: $w / $h")

  def convertTo32F(m: Mat): Mat = {
    val dest = new Mat
    m.convertTo(dest, CV_32F)
    dest
  }

  /**
   * warps image from to make feature extraction's life easier (time intensive call)
   */
  def warp(input: Mat, srcCorners: Mat): Mat = {
    val srcSize = srcCorners.size

    // p("srcSize", srcSize.width, srcSize.height)
    // p("m", mSize.width, mSize.height)
    // p("destSize", destSize.width, destSize.height)
    require(srcSize.width == 2)
    require(srcSize.height == 4)

    warpP(input, getPerspectiveTransform(srcCorners, normalizedCorners))
  }

  def unwarp(input: Mat, destCorners: Mat): Mat = {
    warpP(input, getPerspectiveTransform(normalizedCorners, destCorners))
  }

  def warpP(input: Mat, transformationMatrix: Mat) = {
    val dest = new Mat()
    warpPerspective(input, dest, transformationMatrix, normalizedSize)
    dest
  }


  def findContours(original: Mat, mode: Int, method: Int): MatVector = {
    val contours = new MatVector()
    opencv_imgproc.findContours(original, contours, new Mat, mode, method)
    contours
  }

  /**
   * A Mat which contains lines, with 4 entries
   *
   * @param needle
   * @return
   */
  def has4Sides(needle: Mat): Boolean = {
    val width = needle.size.width
    val height = needle.size.height
    println(s"$width / $height")
    width == 1 && height == 4
    //    needle.size == new Size(1, 4)
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

  def threshold(input: Mat): Mat = {
    val output = new Mat
    opencv_imgproc.threshold(input, output, 30, 255, opencv_imgproc.THRESH_BINARY)
    output
  }


  def filter2D(kernel: Mat)(input: Mat): Mat = {
    val out = new Mat
    opencv_imgproc.filter2D(input, out, -1, kernel)
    out
  }


  case class SpecialParam(center: Point, minArea: Double, maxArea: Double)

  def mkCellSize(sudokuSize: Size): Size = new Size(sudokuSize.width / ssize, sudokuSize.height / ssize)

  def mkRect(i: Int, width: Int, height: Int): Rect = new Rect(Parameters.col(i) * width, Parameters.row(i) * height, width, height)

  def mkRect(i: Int, s: Size): Rect = new Rect(Parameters.col(i) * s.width, Parameters.row(i) * s.height, s.width, s.height)


}