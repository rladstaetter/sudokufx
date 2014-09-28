package net.ladstatt.apps.sudoku


import java.io.File
import java.util.concurrent.TimeUnit

import net.ladstatt.apps.sudoku.SudokuAlgos.BruteForceSolver
import net.ladstatt.core.{CanLog, HasDescription, Utils}
import net.ladstatt.opencv.OpenCVUtils
import org.opencv.core._
import org.opencv.imgproc.Imgproc

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try, Success}


object TemplateDetectionStrategy extends OpenCVUtils {

  val description = "template detection"
  val (templateWidth, templateHeight) = (25.0, 50.0)
  val templateSize = new Size(templateWidth, templateHeight)

  val templateLibrary: Map[Int, Mat] = {
    (1 to 9).map {
      case i => i -> toMat(Templates.templatesAsByteArray(i),
        templateSize.width.toInt,
        templateSize.height.toInt)
    }.toMap
  }


  /**
   * given a template library, match the given contour to find the best match. this function takes around 1 ms
   *
   * @return
   */
  // TODO change return type to Future[(SNum,SQuality)] -> Number
  def detect(someCandidate: Option[Mat]): Future[(SNum, SHitQuality)] = {
    val tmp = Promise[(SNum, SHitQuality)]()

    someCandidate match {
      case None => tmp.success((0, 0))
      case Some(candidate) =>
        tmp.tryCompleteWith({
          //  for (f <- persist(candidate, new File(UUID.randomUUID().toString + ".png"))) yield f
          val resizedCandidateF = resize(candidate, templateSize) // since templates are 25 x 50
          val matchHaystack = matchTemplate(resizedCandidateF, _: Future[(Int, Mat)])

          for {s <- Future.sequence(for {templateEntry <- templateLibrary} yield
            for {y <- matchHaystack(Future.successful(templateEntry))} yield y)} yield s.toSeq.sortWith((a, b) => a._2 < b._2).head

        })
    }

    tmp.future
  }


  // override lazy val detect: (Option[Mat]) => Future[(SNum, SHitQuality)] = withTemplateMatching

}



//case class FrameHistory(index: Int, file: File, result: FrameResult)

// TODO remove: replace with Array in SudokuHistory
case class SCell(value: Int, quality: Double, data: Mat) {
  assert(0 <= value && value <= 9)
  assert(quality >= 0)

}

object SudokuAlgos extends SudokuOpenCVUtils {

  /**
   * The following code is the first google hit for "scala sudoku solver", adapted to compile with scala 2.10
   * I hope the author doesn't mind me reusing the code.
   *
   * http://scala-programming-language.1934581.n4.nabble.com/25-lines-Sudoku-solver-in-Scala-td1987506.html
   *
   * Also don't miss the very nice essay from peter norvig on solving sudokus
   *
   * http://norvig.com/net.ladstatt.apps.sudoku.html
   *
   */
  object BruteForceSolver extends HasDescription with Utils with CanLog {

    val description = "default"

    def printTime(t: Long) = println(s"solved in $t ms.")

    /**
     * give this function a net.ladstatt.apps.sudoku in the form
     *
     * 200080300
     * 060070084
     * 030500209
     * 000105408
     * 000000000
     * 402706000
     * 301007040
     * 720040060
     * 004010003
     *
     * and it will return the solved net.ladstatt.apps.sudoku (with zeros)
     *
     */
    def solve(stringRep: String, maxDuration: Long = 100l): Option[String] = time({
      val before = System.currentTimeMillis()
      var cnt = 0
      def isCancelled = {
        cnt = cnt + 1
        val duration = (System.currentTimeMillis() - before)
        if (duration > maxDuration) {
          System.err.println(s"---> CANCEL (timeout: $duration ms, count $cnt.)")
          true
        } else false
      }

      val stringRep2 = """000000000
                         |040070300
                         |059060001
                         |006240000
                         |000050400
                         |008790000
                         |034000008
                         |000080906
                         |000400000""".stripMargin

      // The board is represented by an array of strings (arrays of chars),
      // held in a global variable mx. The program begins by reading 9 lines
      // of input to fill the board
      val mx: Array[Array[Char]] = stringRep.stripMargin.split("\n").map(_.trim.toArray)

      var solution = new ListBuffer[String]()

      def print = {
        mx map (carr => {
          solution.append(new String(carr))
        })
      }

      // The test for validity is performed by looping over i=0..8 and
      // testing the row, column and 3x3 square containing the given
      // coordinate
      def invalid(i: Int, x: Int, y: Int, n: Char): Boolean =
        i < 9 && (mx(y)(i) == n || mx(i)(x) == n ||
          mx(y / 3 * 3 + i / 3)(x / 3 * 3 + i % 3) == n || invalid(i + 1, x, y, n))

      // Looping over a half-closed range of consecutive integers [l..u)
      // is factored out into a higher-order function
      def fold(f: (Int, Int) => Int, accu: Int, l: Int, u: Int): Int =
        if (l == u) accu else fold(f, f(accu, l), l + 1, u)

      // The search function examines each position on the board in turn,
      // trying the numbers 1..9 in each unfilled position
      // The function is itself a higher-order fold, accumulating the value
      // accu by applying the given function f to it whenever a solution m
      // is found
      def search(x: Int, y: Int, f: (Int) => Int, accu: Int): Int = (x, y) match {
        case (9, yy) if (!isCancelled) => search(0, y + 1, f, accu) // next row
        case (0, 9) if (!isCancelled) => f(accu) // found a solution
        case (xx, yy) if (!isCancelled) => {
          if (mx(y)(x) != '0') {
            search(x + 1, y, f, accu)
          } else {
            fold((accu: Int, n: Int) =>
              if (invalid(0, x, y, (n + 48).toChar)) accu
              else {
                mx(y)(x) = (n + 48).toChar
                val newaccu = search(x + 1, y, f, accu)
                mx(y)(x) = '0'
                newaccu
              }, accu, 1, 10)
          }
        }
        case _ => {
          throw new RuntimeException("Was cancelled.")
        }
      }

      // The main part of the program uses the search function to accumulate
      // the total number of solutions
      Try {
        search(0, 0, i => {
          print
          i + 1
        }, 0)

        solution.toList.mkString("\n")
        // thats all ;-)
      } match {
        case Success(s) => Some(s)
        case Failure(e) => None
      }
    }, printTime)

  }


  object MockSolver extends HasDescription {
    val description = "mock test solver"

    def solve(s: String)(log: String => Unit): String = {
      """245981376
        |169273584
        |837564219
        |976125438
        |513498627
        |482736951
        |391657842
        |728349165
        |654812793""".stripMargin
    }

    def solve2(s: String)(log: String => Unit): Future[String] = Future {
      """000000000
        |040070300
        |059060001
        |006240000
        |000050400
        |008790000
        |034000008
        |000080906
        |000400000""".stripMargin
    }
  }

  import Parameters._

  sealed trait ProcessingStage

  case object InputStage extends ProcessingStage

  case object GrayedStage extends ProcessingStage

  case object BlurredStage extends ProcessingStage

  case object ThresholdedStage extends ProcessingStage

  case object InvertedStage extends ProcessingStage

  case object DilatedStage extends ProcessingStage

  case object ErodedStage extends ProcessingStage

  case object SolutionStage extends ProcessingStage


  abstract trait SolverStrategy extends HasDescription {

    def solve(s: String)(log: String => Unit): String
  }







  def calcBlockSize(input: Mat): (Double, Double) = {
    val (matWidth, matHeight) = (input.size().width, input.size.height)
    val (blockWidth, blockHeight) = ((matWidth / 9), (matHeight / 9))
    (blockWidth, blockHeight)
  }


  // only search for contours in a subrange of the original cell to get rid of possible border lines
  def specialize(cellRawData: Mat): Future[(Mat, Point, Double, Double)] =
    execFuture {
      val (width, height) = (cellRawData.size.width, cellRawData.size.height)
      val cellData = new Mat(cellRawData, new Range((height * 0.1).toInt, (height * 0.9).toInt), new Range((width * 0.1).toInt, (width * 0.9).toInt))
      val cellArea = cellData.size().area
      val (minArea, maxArea) = (0.15 * cellArea, 0.5 * cellArea)
      val (centerX, centerY) = (cellData.size.width / 2, cellData.size.height / 2)
      (cellData, new Point(centerX, centerY), minArea, maxArea)
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
      (cellData, center, minArea, maxArea) <- specialize(cell)
      a <- preprocess2(cellData)
      b <- preprocess2(cellData)
    } yield
      findCellContour(a, b, center, minArea, maxArea)
  }


  def subMat(mat: Mat, rect: Rect): Future[Mat] =
    execFuture {
      mat.submat(rect)
    }

  def detectCells(colorWarped: Mat, detectionMethod: DetectionMethod): Seq[Future[(Pos, SCell)]] = {
    val (blockWidth, blockHeight) = calcBlockSize(colorWarped)
    val size = new Size(blockWidth, blockHeight)
    for (p <- positions) yield
      for {
        coloredSubMat <- subMat(colorWarped, mkRect(p, size))
        contour <- extractContour(coloredSubMat)
        (value, quality) <- detectionMethod(contour)
      } yield {
        p -> SCell(value, quality, coloredSubMat)
      }
  }


  /*
      val someOtherMatrix = digitLibrary.find(m => m.isDefined)
      if (someOtherMatrix.isDefined) {
        val otherMatrix = someOtherMatrix.get
        val (size, matType) = (otherMatrix.size, otherMatrix.`type`())
        val mat = new Mat(size.height.toInt, size.width.toInt, matType).setTo(new Scalar(255, 255, 255))
        Core.putText(mat, number.toString, new Point(size.width * 0.3, size.height * 0.9), Core.FONT_HERSHEY_TRIPLEX, 2, new Scalar(0, 0, 0))
        digitLibrary(number) = mat
        Some(mat)
      } else None
    }   */


}


trait SudokuOpenCVUtils extends OpenCVUtils {

  def mkRect(pos: Pos, c: Mat): Rect = {
    assert(c != null)
    val (width, height) = (c.size.width, c.size.height)
    val (x, y) = (pos._2 * width, pos._1 * height)
    new Rect(new Point(x, y), c.size)
  }

  def mkRect(pos: Pos, size: Size): Rect = {
    new Rect(new Point(pos._2 * size.width, pos._1 * size.height), size)
  }

  def preprocess2(input: Mat): Future[Mat] = {
    for {
      equalized <- equalizeHist(input)
      blurred <- blur(equalized)
      thresholded <- threshold(blurred)
      inverted <- bitwiseNot(thresholded)
    } yield inverted
  }

  def imageIOChain(input: Mat): Future[ImageIOChain] = {
    for {
      working <- copySrcToDestWithMask(input, new Mat, input)
      grayed <- toGray(working)
      blurred <- blur(grayed)
      thresholdApplied <- adaptiveThreshold(blurred)
      inverted <- bitwiseNot(thresholdApplied)
      dilated <- dilate(inverted)
      eroded <- erode(inverted)
      //  dilated <- dilate(thresholdApplied)
      //  inverted <- bitwiseNot(dilated)
      corners <- Future.successful(mkCorners(input))
    } yield ImageIOChain(input, working, grayed, blurred, thresholdApplied, inverted, dilated, eroded, corners)
  }

}


object Parameters {

  val ssize = 9
  val range = 0 until ssize
  val positions: IndexedSeq[Pos] = for {r <- range
                                        c <- range} yield (r, c)
  val digitRange = 0 to ssize
  val colorRange = 0 to 256 by 16
  private val leftRange: Seq[Int] = Seq(0, 1, 2)
  private val middleRange: Seq[Int] = Seq(3, 4, 5)
  private val rightRange: Seq[Int] = Seq(6, 7, 8)
  val sectors: Seq[Seq[Int]] = Seq(leftRange, middleRange, rightRange)

}


