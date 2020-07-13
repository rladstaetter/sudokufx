package net.ladstatt.sudoku

import java.nio.file.{Files, Path}
import java.nio.{ByteBuffer, DoubleBuffer, FloatBuffer, IntBuffer}
import java.util.UUID

import net.ladstatt.core.CanLog
import org.apache.commons.io.IOUtils
import org.bytedeco.javacpp.{BytePointer, FloatPointer}
import org.bytedeco.opencv.global._
import org.bytedeco.opencv.global.opencv_core._
import org.bytedeco.opencv.global.opencv_imgproc._
import org.bytedeco.opencv.opencv_core._

import scala.math.{abs, atan2}


/**
 * Contains functions which are backed by JavaCV's API to OpenCV.
 */
object JavaCV extends CanLog {

  lazy val EmptyCorners = new Point2f

  val leftTop = new Point(1, 1)
  val blackScalar = new Scalar(0)


  def removeBorderArtefacts(m: Mat): Mat = {
    val labels = new Mat
    val stats = new Mat
    val centroids = new Mat
    opencv_imgproc.connectedComponentsWithStats(m, labels, stats, centroids, 8, opencv_core.CV_32S)
    // opencv_core.greaterThan(stats, 40000).asMat()
    // val match2 = opencv_core.equals(labels, 1).asMat
    //printIntBf(labels)
    //    printByteBf(match2)
    //val labelCount: Int = stats.rows
    //println("size:" + stats.size.width() + "/" + stats.size.height())
    //println(labelCount)

    val notTouchingBorder = (for {i <- 0 until stats.rows
                                  ibf = stats.row(i).createBuffer[IntBuffer]()
                                  } yield Label(i, ibf)).filter(!_.touches(m.size.width, m.size.height))


    val toBeAnalyzed =
      if (notTouchingBorder.size > 1) {
        // logWarn("Found more than one label, choosing the one with biggest area")
        Seq(notTouchingBorder.sortWith((a, b) => a.a > b.a).head)
      } else notTouchingBorder

    val hitters =
      toBeAnalyzed.headOption match {
        case None =>
          opencv_core.equals(labels, -1).asMat
        case Some(l) => opencv_core.equals(labels, l.i).asMat
      }
    // printByteBf(hitters)
    val out = new Mat
    m.copyTo(out, hitters)
    out

  }

  def floodFillCorners(m: Mat): Mat = {
    val s = m.size

    if (s.width > 1 && s.height > 1) {
      val a = floodFill(m, leftTop, blackScalar)
      val leftLow = new Point(s.height - 1, 1)
      val b = floodFill(m, leftLow, blackScalar)

      /*
            val rightTop = new Point(1,s.width - 40)
            val c = floodFill(m, rightTop, blackScalar)
            val rightLow = new Point(s.height - 1, 1)
            val d = floodFill(m, rightLow, blackScalar)
      */

      /*
      val floodFilled: Mat =
        //Seq(leftTop).foldLeft(m) {
           Seq(leftTop, rightTop, leftLow, rightLow).foldLeft(m) {
          case (acc, point) => floodFill(acc, point, blackScalar)
        }
      */
      m
    } else {
      logError("check input for floodFillCorners!")
      m
    }
  }

  def floodFill(m: Mat, p: Point, color: Scalar): Mat = {
    opencv_imgproc.floodFill(m, p, color)
    m
  }

  def imdecode(b: ByteBuffer, width: Int, height: Int): Mat = {
    val mat = new Mat(width, height, opencv_core.CV_8UC1)
    new Mat(width, height, CV_8U, new BytePointer(b))
    mat.data(new BytePointer(b))
    opencv_imgcodecs.imdecode(mat, opencv_imgcodecs.IMREAD_UNCHANGED)
    mat
  }

  val erodeParam: Mat = opencv_imgproc.getStructuringElement(opencv_imgproc.MORPH_CROSS,
    new Size(1, 1),
    new Point(0, 0))

  def erode(input: Mat): Mat = {
    val output = new Mat
    opencv_imgproc.erode(input, output, erodeParam)
    output
  }

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
/*
  def loadMat2(clazz: Class[_], matCp: MatCp, codec: Int = opencv_imgcodecs.IMREAD_COLOR): Mat = {
    val is = matCp.inputStream(clazz)
    var nRead = 0
    val data = new Array[Byte](16 * 1024)
    val buffer = new Nothing
    while ( {
      (nRead = is.read(data, 0, data.length)) != -1
    }) buffer.write(data, 0, nRead)
    val bytes = buffer.toByteArray
    val mat = imdecode(new Nothing(bytes), CV_LOAD_IMAGE_UNCHANGED)
  }
  */
  /** loads a mat from classpath */
  def loadMat(clazz: Class[_], matCp: MatCp, codec: Int = opencv_imgcodecs.IMREAD_COLOR): Mat = synchronized {
    if (matCp.existsUsingClassLoader(clazz)) {

      val stream = matCp.inputStream(clazz)
      val bytes = IOUtils.toByteArray(stream)
      stream.close()
      val buffer = ByteBuffer.wrap(bytes)
      opencv_imgcodecs.imdecode(new Mat(new BytePointer(buffer)), codec)
    } else {
      logError(s"Classpath entry '${matCp.value}' not found.")
      new Mat()
    }
  }

  def loadMat(path: Path): Mat = {
    if (Files.exists(path)) {
      opencv_imgcodecs.imread(path.toAbsolutePath.toString)
    } else {
      throw new RuntimeException(path.toAbsolutePath.toString + " does not exist.")
    }
  }


  def writeMat(target: Path, mat: Mat): Boolean = {
    if (Sudoku.writeFiles) synchronized {
      Files.createDirectories(target.getParent)
      val path = target.toAbsolutePath.toString
      val r = opencv_imgcodecs.imwrite(path, mat)
      logTrace(s"Wrote $path")
      r
    } else true
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

  def copyTo(canvas: Mat, data: Mat, roi: Rect): Mat = {
    val cellTarget = new Mat(canvas, roi)
    data.copyTo(cellTarget)
    canvas
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
    val p = new FloatPointer(0, 0, width, 0, width, height, 0, height)
    new Mat(new Size(2, 4), opencv_core.CV_32F, p)
  }


  def toMat(buffer: Array[Byte], size: Size): Mat = {
    val bytePointer = new BytePointer(ByteBuffer.wrap(buffer))
    val m = new Mat(size, CV_8U, bytePointer)
    m
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

  val dilateAnchor = new Point(-1, -1)
  val dilateScalar = new Scalar(morphologyDefaultBorderValue())

  def dilate(input: Mat): Mat = {
    val mat = new Mat
    opencv_imgproc.dilate(input, mat, JavaCV.Kernel, dilateAnchor, 2, BORDER_CONSTANT, dilateScalar)
    mat
  }

  def norm(operation: String
           , id: String
           , frameNr: Int
           , pos: Int
           , path: Path
           , mat: Mat): Mat = {
    val b = gaussianblur(mat)
    writeMat(s"7_gaussianblur-$operation", id, frameNr, pos, path, b)
    val dilated = dilate(b)
    writeMat(s"8_dilated-$operation", id, frameNr, pos, path, b)
    val thresholded = adaptiveThreshold(dilated, 255, 9)
    writeMat(s"9_thresholded-$operation", id, frameNr, pos, path, b)
    thresholded
  }

  def writeMat(operation: String
               , id: String
               , frameNr: Int
               , pos: Int
               , parentFolder: Path
               , res: Mat): Boolean = {
    val path = parentFolder.resolve(frameNr.toString).resolve(pos.toString).resolve(operation).resolve(s"$id-${
      UUID.randomUUID().toString
    }.png")
    writeMat(path, res)
  }

  def doitWith(id: String, frameNr: Int, pos: Int, name: String, op: Mat => Mat, parentFolder: Path)(mat: Mat): Mat = {
    val res: Mat = op(mat)
    JavaCV.writeMat(name, id, frameNr, pos, parentFolder, res)
    res
  }

  /**
   * Returns detected number and quality for a candidate
   *
   * @return
   */
  def matchTemplate(candidate: Mat
                    , template: Mat
                    , id: String
                    , pos: Int
                    , path: Path
                    , number: Int): (Int, Double) = {


    // val c = norm("norm1", id, pos, path, candidate)
    // val needle = norm("norm2", id, pos, path, withNeedle)

    val width = candidate.cols - template.cols + 1
    val height = candidate.rows - template.rows + 1

    val resultImage = new Mat(width, height, CV_32FC1)
    opencv_imgproc.matchTemplate(candidate, template, resultImage, opencv_imgproc.TM_SQDIFF)
    val dbf = DoubleBuffer.wrap(Array[Double](1))
    opencv_core.minMaxLoc(resultImage, dbf)
    (number, dbf.get(0))
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
  def warp(input: Mat, srcCorners: Mat): Mat = synchronized {
    /*
      val srcSize = srcCorners.size

      // p("srcSize", srcSize.width, srcSize.height)
      // p("m", mSize.width, mSize.height)
      // p("destSize", destSize.width, destSize.height)
      require(srcSize.width == 2)
      require(srcSize.height == 4)
  */
    val transformationMatrix = opencv_imgproc.getPerspectiveTransform(srcCorners, Parameters.normalizedCorners)
    val dest = new Mat()
    opencv_imgproc.warpPerspective(input, dest, transformationMatrix, Parameters.size1280x720)
    dest
  }

  def unwarp(input: Mat, destCorners: Mat): Mat = {
    warpP(input, getPerspectiveTransform(Parameters.normalizedCorners, destCorners))
  }

  def warpP(input: Mat, transformationMatrix: Mat): Mat = {
    val dest = new Mat()
    opencv_imgproc.warpPerspective(input, dest, transformationMatrix, Parameters.size1280x720)
    dest
  }


  def findContours(original: Mat, mode: Int, method: Int): MatVector = {
    val contours = new MatVector()
    opencv_imgproc.findContours(original, contours, new Mat, mode, method)
    contours
  }

  def gaussianblur(input: Mat): Mat = {
    val mat = new Mat()
    opencv_imgproc.GaussianBlur(input, mat, new Size(11, 11), 0)
    mat
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
    width == 1 && height == 4
    //    needle.size == new Size(1, 4)
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

  /**
   * goal is to reorganize inputs in a way that all 4 coordinate pairs are returned in following ordering:
   *
   * <ul>
   * <li>left upper corner</li>
   * <li>right uper corner</li>
   * <li>right lower corner</li>
   * <li>left lower corner</li>
   * </ul>
   *
   * @param m
   * @return
   */
  def sortCorners(m: Mat): Mat = {
    val ps: Seq[PInt] = JavaCV.extractIntPoints(m)

    // idea: the top left corner has the smallest, the bottom right corner the biggest distance to (0,0).
    // hence we have two points. for the other two points we just say that B has a smaller y coordinate
    // than point D. Should work for most of the cases
    val sortByDist = ps.sortWith((a, b) => a.dist < b.dist)
    val A = sortByDist.head
    val C = sortByDist.reverse.head
    val rest = ps.toSet -- Set(A, C)
    val sortByY = rest.toSeq.sortWith((a, b) => a.y < b.y)
    val B = sortByY.head
    val D = sortByY.tail.head

    val fp = new FloatPointer(A.x, A.y, B.x, B.y, C.x, C.y, D.x, D.y)
    //println(s"!!!: ${A.x}, ${A.y}, ${B.x}, ${B.y}, ${C.x}, ${C.y}, ${D.x}, ${D.y}")
    new Mat(new Size(2, 4), opencv_core.CV_32F, fp)
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
    // opencv_imgproc.threshold(input, output, 30, 255, opencv_imgproc.THRESH_BINARY)
    opencv_imgproc.threshold(input, output, 30, 255, opencv_imgproc.THRESH_BINARY)
    output
  }


  def filter2D(kernel: Mat)(input: Mat): Mat = {
    val out = new Mat
    opencv_imgproc.filter2D(input, out, -1, kernel)
    out
  }


  def mkCellSize(sudokuSize: Size): Size = new Size(sudokuSize.width / Parameters.ssize, sudokuSize.height / Parameters.ssize)

  def mkRect(i: Int, width: Int, height: Int): Rect = new Rect(Parameters.col(i) * width, Parameters.row(i) * height, width, height)

  def mkRect(i: Int, s: Size): Rect = new Rect(Parameters.col(i) * s.width, Parameters.row(i) * s.height, s.width, s.height)


}