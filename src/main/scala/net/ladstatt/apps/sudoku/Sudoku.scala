package net.ladstatt.apps.sudoku


import net.ladstatt.core.{HasDescription, Utils}
import net.ladstatt.opencv.OpenCV
import org.opencv.core._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.util.{Failure, Success, Try}


object TemplateDetectionStrategy {


  val templateLibrary: Map[Int, Mat] = {
    (1 to 9).map {
      case i => i -> OpenCV.toMat(Templates.templatesAsByteArray(i), Parameters.templateSize)
    }.toMap
  }
  /*
val writeToDisk : Unit = {
for ((i,m) <- templateLibrary) {
OpenCV.persist(m, new File(s"src/main/resources/net/ladstatt/apps/sudokufx/t_$i.png"))
}
}    */

  /**
   * given a template library, match the given contour to find the best match. this function takes around 1 ms
   *
   * @return
   */
  def detectNumber(candidate: Mat): Future[(SNum, SHitQuality)] = {
    val resizedCandidate = OpenCV.resize(candidate, Parameters.templateSize) // since templates are 25 x 50
    val matchHaystack = OpenCV.matchTemplate(resizedCandidate, _: Mat, _: Int)

    val result =
      for {s <- Future.sequence(for {(number, needle) <- templateLibrary} yield
        for {(number, quality) <- matchHaystack(needle, number)} yield (number, quality))
      } yield s.toSeq.sortWith((a, b) => a._2 < b._2).head

    //  OpenCV.persist(candidate,new File(s"${result._1}"))
    result
  }


  // override lazy val detect: (Option[Mat]) => Future[(SNum, SHitQuality)] = withTemplateMatching

}


// TODO remove: replace with Arrays in SudokuState
// digitSolutionData : Array[Mat]
// digitSolutionQuality : Array[Double]
case class SCell(value: Int, quality: Double, data: Mat) {
  assert(0 <= value && value <= 9, s"value: $value")
  assert(quality >= 0)

}

object SudokuAlgos {

  import net.ladstatt.opencv.OpenCV._

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
  object BruteForceSolver extends HasDescription with Utils {

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
    def solve(mx: SudokuDigitSolution, maxDuration: Long = 100l): Option[SudokuDigitSolution] = time({
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

      def copy(s: SudokuDigitSolution): SudokuDigitSolution = {
        for (l <- s) yield l.clone()
      }

      // The board is represented by an array of strings (arrays of chars),
      // held in a global variable mx. The program begins by reading 9 lines
      // of input to fill the board
      val solution: Array[Array[Char]] = Array.fill(Parameters.ssize)(Array.fill(Parameters.ssize)('A'))

      def populateSolution() = {
        for (li <- 0 until mx.size) {
          solution(li) = mx(li).clone()
        }
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
      def search(x: Int, y: Int, f: Int => Int, accu: Int): Int = (x, y) match {
        case (9, yy) if (!isCancelled) => search(0, y + 1, f, accu) // next row
        case (0, 9) if (!isCancelled) => f(accu) // found a solution
        case (xx, yy) if (!isCancelled) => {
          if (mx(y)(x) != '0') {
            search(x + 1, y, f, accu)
          } else {
            fold((accu: Int, n: Int) =>
              if (invalid(0, x, y, (n + 48).toChar)) {
                accu
              }
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
          //println(i)
          populateSolution()
          //???
          i + 1
        }, 0)
        solution
      } match {
        case Success(s) => Some(s)
        case Failure(e) => {
          println(e.getMessage)
          None
        }
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

  import net.ladstatt.apps.sudoku.Parameters._

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

  def cellSize(size: Size): Size = new Size(size.width / ssize, size.height / ssize)

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
      a <- preprocess2(cellData) // TODO what is going on here?
      b <- preprocess2(cellData)
    } yield
      findCellContour(a, b, center, minArea, maxArea)
  }


  def detectCell(cell: Mat): Future[SCell] = {
    import net.ladstatt.apps.sudoku.TemplateDetectionStrategy.detectNumber
    for {
      contour <- extractContour(cell)
      (value, quality) <- contour.map(detectNumber(_)).getOrElse(Future.successful((0, 0.0)))
    } yield {
      SCell(value, quality, cell)
    }
  }

  def detectCells(searchArea: Mat, rects: Seq[Rect]): Seq[Future[SCell]] = {
    rects.map(r => detectCell(searchArea.submat(r)))
    //for (r <- rects) yield detectCell(searchArea.submat(r))
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
    } yield ImageIOChain(working, grayed, blurred, thresholdApplied, inverted, dilated, eroded)
  }

}


object Parameters {

  val ssize = 9
  val cellCount = ssize * ssize

  val range = 0 until ssize
  val digitRange = 0 to ssize

  val positions = 0 until cellCount

  val colorRange = 0 to 256 by 16
  private val leftRange: Seq[Int] = Seq(0, 1, 2)
  private val middleRange: Seq[Int] = Seq(3, 4, 5)
  private val rightRange: Seq[Int] = Seq(6, 7, 8)
  val sectors: Seq[Seq[Int]] = Seq(leftRange, middleRange, rightRange)

  val (templateWidth, templateHeight) = (25.0, 50.0)
  val templateSize = new Size(templateWidth, templateHeight)
  lazy val sudokuSize = new Size(templateWidth * ssize, templateHeight * ssize)

  lazy val templateCorners = OpenCV.mkCorners(sudokuSize)

  def row(i: SIndex): Int = i / 9

  def col(i: SIndex): Int = i % 9
}


