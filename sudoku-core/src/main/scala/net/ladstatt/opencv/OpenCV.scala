package net.ladstatt.opencv

import java.io.File

import net.ladstatt.core.{CanLog, SystemEnv}
import net.ladstatt.sudoku._
import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try


object Debug {

  val Active = false

  def writeDebug(sCandidate: SCandidate) = {
    val parent = new File(s"/Users/lad/Documents/sudokufx/sudoku-core/target/test-classes/net/ladstatt/sudoku/normalized/${sCandidate.nr}")
    parent.mkdirs()
    //println("InputCorners:" + sCandidate.framePipeline.corners)
    //println("CellWidth  :" + sCandidate.sRectangle.cellWidth + "/" + sCandidate.sRectangle.cellHeight)
    // write sudoku canvas
    sCandidate.pipeline.persist(parent)
    writeMat(new File(parent, s"normalized"), sCandidate.sRectangle.normalized)
  }

  def writeMat(target: File, mat: Mat): Boolean = {
    println(target)
    Imgcodecs.imwrite(target.getAbsolutePath + ".png", mat)
  }


}

/**
  * various opencv related stuff
  */
object OpenCV extends CanLog {

  import Parameters._

  lazy val EmptyCorners = new MatOfPoint2f

  lazy val Kernel: Mat = {
    val k = new Mat(3, 3, CvType.CV_8U)
    k.put(0, 0, Array[Byte](0, 1, 0, 1, 1, 1, 0, 1, 0))
    k
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
    Imgproc.rectangle(canvas, rect.tl(), rect.br(), color, thickness)
  }

  /**
    * Given a list of curves, this function returns the curve with the biggest area which is described by the
    * contour.
    *
    * @param curveList
    * @return
    */
  def extractCurveWithMaxArea(curveList: Seq[MatOfPoint]): (Double, MatOfPoint) = {
    if (curveList.isEmpty)
      (0.0, new MatOfPoint)
    else
      curveList.foldLeft[(Double, MatOfPoint)]((0.0, new MatOfPoint)) {
        case ((area, c), curve) =>
          val cArea = Imgproc.contourArea(curve)
          if (cArea >= area) (cArea, curve) else (area, c)
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
      atan2(b.y - a.y, b.x - a.x) * 180 / scala.math.Pi
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

  def mkCorners(size: Size): MatOfPoint2f = {
    val (width, height) = (size.width, size.height)
    new MatOfPoint2f(new Point(0, 0),
      new Point(width, 0),
      new Point(width, height),
      new Point(0, height))
  }


  def toMat(buffer: Array[Byte], size: Size): Mat = {
    val m = new Mat(size, CvType.CV_8UC1)
    m.put(0, 0, buffer)
    m
  }

  def persist(mat: Mat, file: File): Try[File] =
    Try {
      logWithTimer(s"Wrote ${file.getAbsolutePath}", {
        if (!Imgcodecs.imwrite(file.getAbsolutePath, mat)) {
          throw new RuntimeException(s"Could not save to file $file")
        } else {
          file
        }
      })
    }


  def alphaBlend(src: Mat, alpha: Mat): Mat = {
    val channels = new java.util.ArrayList[Mat]()
    Core.split(src, channels)
    channels.add(alpha)
    val merged = new Mat
    Core.merge(channels, merged)
    merged
  }

  def mkMatWithCurve(image: Mat, curve: MatOfPoint2f, color: Scalar, thickness: Int): Future[Mat] = {
    mkMatWithCurve(image, curve.toList.toList, color, thickness)
  }

  def mkMatWithCurve(image: Mat, points: List[Point], color: Scalar, thickness: Int): Future[Mat] =
    Future {
      if (points.size > 2) {
        for (linePoints <- points.sliding(2)) {
          Imgproc.line(image, linePoints(0), linePoints(1), color, thickness)
        }
      }
      image
    }

  /**
    * wraps equalizeHist from Imgproc
    *
    * @param input
    * @return
    */
  def equalizeHist(input: Mat): Future[Mat] =
    Future {
      val output = new Mat
      Imgproc.equalizeHist(input, output)
      output
    }


  def norm(mat: Mat): Future[Mat] = {
    for {
      b <- gaussianblur(mat)
      dilated <- dilate(b, OpenCV.Kernel)
      thresholded <- adaptiveThreshold(dilated, 255, 9)
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
          val resultImage = new Mat(width, height, CvType.CV_32FC1)
          Imgproc.matchTemplate(c, needle, resultImage, Imgproc.TM_SQDIFF)
          val minMaxResult = Core.minMaxLoc(resultImage)
          //        OpenCV.persist(c, new File(s"target/${number}_${minMaxResult.minVal}_candidate_.png"))
          //        OpenCV.persist(needle, new File(s"target/${number}_${minMaxResult.minVal}_needle_.png"))
          (number, minMaxResult.minVal)
        }
    result
  }

  def resize(s: Mat, size: Size): Mat = {
    val dest = new Mat()
    Imgproc.resize(s, dest, size)
    dest
  }

  def resizeFuture(source: Mat, size: Size): Future[Mat] = Future(resize(source, size))


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
    * warps image from to make feature extraction's life easier (time intensive call)
    */
  def warp(input: Mat, srcCorners: MatOfPoint2f, destCorners: MatOfPoint2f): Mat = {
    require(srcCorners.toList.size == 4)
    require(destCorners.toList.size == 4)

    val transformationMatrix: Mat = Imgproc.getPerspectiveTransform(srcCorners, destCorners)

    val dest = new Mat()
    Imgproc.warpPerspective(input, dest, transformationMatrix, input.size())
    dest
  }

  // input mat will be altered by the findContours(...) function
  def findContours(original: Mat, mode: Int, method: Int): Seq[MatOfPoint] = {
    val input = copyMat(original)
    val contours = new java.util.ArrayList[MatOfPoint]()
    Imgproc.findContours(input, contours, new Mat, mode, method)
    contours
  }

  def has4Sides(needle: MatOfPoint2f): Boolean = needle.size == new Size(1, 4)

  def mkApproximation(curve: MatOfPoint2f, epsilon: Double = 0.02): MatOfPoint2f = {
    val arcLength = Imgproc.arcLength(curve, true)
    val approxCurve = new MatOfPoint2f
    Imgproc.approxPolyDP(curve, approxCurve, epsilon * arcLength, true)
    approxCurve
  }

  def contourPredicate(sp: SpecialParam)(c: MatOfPoint): Boolean = {
    val boundingRect = Imgproc.boundingRect(c)
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
  def findBestFit(contours: Seq[MatOfPoint], predicate: MatOfPoint => Boolean): Option[Rect] = {

    contours.foldLeft[Option[(Double, MatOfPoint)]](None) {
      case (acc, c) =>
        if (predicate(c)) {
          val area = Imgproc.contourArea(c)
          acc match {
            case None => Some((area, c))
            case Some((a, cc)) =>
              if (area > a) Some((area, c)) else acc
          }
        } else acc
    } map {
      case (_, contour) => Imgproc.boundingRect(contour)
    }

  }


  def findCellContour(original: Mat,
                      specialParam: SpecialParam,
                      contourMode: Int,
                      contourMethod: Int): Option[Mat] = {
    val contours: Seq[MatOfPoint] = findContours(original, contourMode, contourMethod)
    val predicateFn: (MatOfPoint) => Boolean = contourPredicate(specialParam)
    findBestFit(contours, predicateFn) map original.submat
  }

  /**
    * sort points in following order:
    * topleft, topright, bottomright, bottomleft
    */
  def mkSortedCorners(points: MatOfPoint2f): Seq[Point] = {
    val pointsAsList = points.toList
    val sortBySum = pointsAsList.sortWith((l, r) => (l.x + l.y) < (r.x + r.y))
    val sortByDifference = pointsAsList.sortWith((l, r) => (l.y - l.x) < (r.y - r.x))
    val (topleft, bottomright) = (sortBySum.head, sortBySum.reverse.head)
    val (topright, bottomleft) = (sortByDifference.head, sortByDifference.reverse.head)
    Seq(topleft, topright, bottomright, bottomleft)
  }

  def adaptiveThreshold(input: Mat,
                        maxValue: Double = 255,
                        blockSize: Int = 5,
                        c: Double = 2,
                        adaptiveMethod: Int = Imgproc.ADAPTIVE_THRESH_MEAN_C): Future[Mat] =
    Future {
      val thresholded = new Mat()
      Imgproc.adaptiveThreshold(input, thresholded, maxValue, adaptiveMethod, Imgproc.THRESH_BINARY, blockSize, c)
      thresholded
    }

  def threshold(input: Mat): Future[Mat] =
    Future {
      val output = new Mat
      Imgproc.threshold(input, output, 30, 255, Imgproc.THRESH_BINARY)
      output
    }

  def bitwiseNot(input: Mat): Future[Mat] =
    Future {
      val output = new Mat
      Core.bitwise_not(input, output)
      output
    }


  def dilate(input: Mat, kernel: Mat): Future[Mat] =
    Future {
      val output = new Mat
      val anchor = new Point(-1, -1)
      Imgproc.dilate(input, output, kernel, anchor, 2)
      output
    }


  def erode(input: Mat): Future[Mat] =
    Future {
      val output = new Mat
      val ersize = 0.0
      val m = Imgproc.getStructuringElement(Imgproc.MORPH_CROSS,
        new Size(2 * ersize + 1, 2 * ersize + 1),
        new Point(ersize, ersize))
      Imgproc.erode(input, output, m)
      //Imgproc.erode(input, output, mkKernel(3, ArrayBuffer[Byte](0, 1, 0, 1, 1, 1, 0, 1, 0)))
      output
    }


  def gaussianblur(input: Mat): Future[Mat] =
    Future {
      val dest = new Mat()
      Imgproc.GaussianBlur(input, dest, new Size(11, 11), 0)
      dest
    }

  def blur(input: Mat): Future[Mat] =
    Future {
      val dest = new Mat()
      Imgproc.blur(input, dest, new Size(20, 20), new Point(-1, -1))
      dest
    }


  def runtimeNativeLibName =
    if (SystemEnv.runOnMac)
      "../lib/libopencv_java310.so"
    else if (SystemEnv.isX64) {
      "lib/win/x64/opencv_java246.dll"
    } else {
      "lib/win/x86/opencv_java246.dll"
    }

  def loadNativeLib(nativeLibName: => String = runtimeNativeLibName) = {
    val nativeLib = new File(nativeLibName)
    assert(nativeLib.exists, "Could not find %s.".format(nativeLibName))
    System.load(nativeLib.getAbsolutePath())
  }

  def filter2D(kernel: Mat)(input: Mat): Mat = {
    val out = new Mat
    Imgproc.filter2D(input, out, -1, kernel)
    out
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
      Imgproc.cvtColor(input, grayed, Imgproc.COLOR_BGR2GRAY)
      grayed
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
      cell <- toGray(coloredCell)
      (cellData, sp) <- specialize(cell)
      equalized <- equalizeHist(cellData)
      blurred <- gaussianblur(equalized)
      thresholded <- threshold(blurred)
      a <- bitwiseNot(thresholded)
    } yield {
      val someMat = findCellContour(a, sp, Imgproc.RETR_TREE, Imgproc.CHAIN_APPROX_SIMPLE)

      //   someMat foreach (Imgcodecs.imwrite(new File("/Users/lad/Documents/sudokufx/sudoku-core/target/" + UUID.randomUUID().toString + ".png").getAbsolutePath, _))
      someMat
    }
  }

  def mkCellSize(sudokuSize: Size): Size = new Size(sudokuSize.width / ssize, sudokuSize.height / ssize)

  def mkRect(i: Int, width: Int, height: Int): Rect = new Rect(Parameters.col(i) * width, Parameters.row(i) * height, width, height)

  def mkRect(i: Int, s: Size): Rect = new Rect(Parameters.col(i) * s.width.toInt, Parameters.row(i) * s.height.toInt, s.width.toInt, s.height.toInt)


}