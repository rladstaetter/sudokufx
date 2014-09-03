package net.ladstatt.apps.sudoku


import java.io.File
import java.util.concurrent.TimeUnit

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

trait HasDetectionMethod extends HasDescription {

  def detect: DetectionMethod

}


object TemplateDetectionStrategy extends HasDetectionMethod with OpenCVUtils {

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
  def withTemplateMatching(someCandidate: Option[Mat]): Future[(SNum, SHitQuality)] = {
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


  override lazy val detect: (Option[Mat]) => Future[(SNum, SHitQuality)] = withTemplateMatching

}

/**
 * the result for one frame. a frame is a image from the image stream
 */
trait FrameResult

case class ImageIOChain(input: Mat,
                        working: Mat,
                        grayed: Mat,
                        blurred: Mat,
                        thresholded: Mat,
                        inverted: Mat,
                        dilated: Mat,
                        eroded: Mat,
                        corners: MatOfPoint2f) extends FrameResult

case class FrameSuccess(solution: Mat,
                        imageIoChain: ImageIOChain,
                        solutionString: String,
                        detectedCells: Cells,
                        solutionCells: Cells,
                        corners: MatOfPoint2f) extends FrameResult {
  //  println("SOLUTION:")
  //  println("SOLUTION:")
  //  println(solutionString)
  //  println("SOLUTION:")
  //  println("SOLUTION:")
}

case class FrameHistory(index: Int, file: File, result: FrameResult)

// TODO remove: replace with Array in SudokuHistory
case class SCell(value: Int, quality: Double, data: Mat) {
  assert(0 <= value && value <= 9)
  assert(quality >= 0)

}

trait SudokuAlgos extends SudokuOpenCVUtils {

  import SudokuParameters._

  def isValid(solution: Cells): Boolean = {
    val allVals = for ((k, SCell(value, _, _)) <- solution) yield value
    allVals.foldLeft(0)((acc, e) => acc + e) == 405
  }

  def copyTo(data: Mat, canvas: Mat, roi: Rect): Unit = {
    val cellTarget = new Mat(canvas, roi)
    data.copyTo(cellTarget)
  }

  /**
   * paints the solution to the canvas.
   *
   * returns the modified canvas with the solution painted upon.
   */
  def paintSolution(canvas: Mat,
                    history: SudokuState,
                    detectedCells: Cells,
                    solution: Cells): Future[Mat] = {
    execFuture {
      if (isValid(solution)) {
        // only copy cells which are not already known
        solution.filterKeys(p => detectedCells(p).value == 0) foreach {
          case (pos, cell) => copyTo(cell.data, canvas, mkRect(pos, cell.data))
        }
      } else {
        detectedCells foreach {
          case (pos, cell) => {
            paintRect(canvas, mkRect(pos, cell.data), history.color(pos), 3)
          }
        }
      }
      canvas
    }
  }

  def paintRect(canvas: Mat, rect: Rect, color: Scalar, thickness: Int): Unit = {
    Core.rectangle(canvas, rect.tl(), rect.br(), color, thickness)
  }

  /*
  // mutable as hell and ugly as well
  def computeSolution(solvingStrategy: String => Future[String])(history: SudokuState): Future[String] = {
    option(history.getSomeResult)({
      val solverString = toSolverString(history.mkValueIntermediateMatrix)
      Future.successful(solverString)
    },
    result => {
      option(history.getSomeSolution)({
        val f = solvingStrategy(toSolverString(result))
        for (s <- f if isValidSolution(s)) {
          history.setSomeSolution(Some(s))
        }
        f
      },
      s => Future.successful(s))
    })
  }
                    */


  /**
   * This function uses an input image and a detection method to calculate the sudoku.
   *
   * @param input
   * @param detectionMethod
   * @return
   */
  def calc(history: SudokuState,
           input: Mat,
           detectionMethod: DetectionMethod): Future[FrameResult] = {

    val (imageIoChain, detectedCorners) =
      Await.result(for {
        imgIo <- imageIOChain(input)
        corners <- detectSudokuCorners(imgIo.dilated) // if (!detectedCorners.empty)
      } yield (imgIo, corners), Duration(200, TimeUnit.MILLISECONDS))

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline
    if (!detectedCorners.empty) {
      for {colorWarped <- warp(imageIoChain.input, detectedCorners, imageIoChain.corners)
           detectedCells <- Future.sequence(detectCells(colorWarped, detectionMethod))

           //mutatedHistory <- history.mutateHistory(detectedCells.toMap)
           solutionAsString <- history.computeSolution(detectedCells.toMap)
           solutionCells <- history.toSolutionCells(solutionAsString)

           annotatedSolution <- paintSolution(colorWarped, history, detectedCells.toMap, solutionCells) // create the solution mat
           unwarped <- warp(annotatedSolution, mkCorners(annotatedSolution), detectedCorners)
           solution <- copySrcToDestWithMask(unwarped, imageIoChain.working, unwarped) // copy solution mat to input mat
      } yield FrameSuccess(solution, imageIoChain, solutionAsString, detectedCells.toMap, solutionCells, detectedCorners)
    } else {
      Future.successful(imageIoChain)
    }

  }


  def extractCurveWithMaxArea(input: Mat, curveList: Seq[MatOfPoint]): Option[(Double, MatOfPoint)] = {
    val curvesWithAreas =
      (for (curve <- curveList) yield (Imgproc.contourArea(curve), curve)).toSeq
    curvesWithAreas.sortWith((a, b) => a._1 > b._1).headOption
  }


  def detectSudokuCorners(preprocessed: Mat): Future[MatOfPoint2f] = {
    val p = Promise[MatOfPoint2f]()
    p.completeWith(Future {
      val epsilon = 0.02

      extractCurveWithMaxArea(preprocessed, coreFindContours(preprocessed)) match {
        case None => {
          logWarn("Could not detect any curve ... ")
          new MatOfPoint2f()
        }
        case Some((maxArea, c)) => {
          val expectedMaxArea = Imgproc.contourArea(mkCorners(preprocessed)) / 30
          val approxCurve = mkApproximation(new MatOfPoint2f(c.toList: _*), epsilon)
          if (maxArea > expectedMaxArea) {
            if (has4Sides(approxCurve)) {
              val corners = mkSortedCorners(approxCurve)
              if (isSomewhatSquare(corners.toList)) {
                // println("---> superArea: " + Imgproc.contourArea(mkCorners(preprocessed)))
                // println(s"---> maxArea: $maxArea")
                corners
              } else {
                logWarn(s"Detected ${approxCurve.size} shape, but it doesn't look like a sudoku!")
                new MatOfPoint2f()
              }
            } else {
              logWarn(s"Detected only ${approxCurve.size} shape, but need 1x4!")
              new MatOfPoint2f()
            }
          } else {
            logWarn(s"The detected area of interest was too small ($maxArea < $expectedMaxArea).")
            new MatOfPoint2f()
          }
        }
      }
    })
    p.future

  }


  def isSomewhatSquare(corners: Seq[Point]): Boolean = {

    import scala.math.{abs, atan2}

    def calcAngle(a: Point, b: Point) = {
      atan2(b.y - a.y, b.x - a.x) * 180 / scala.math.Pi
    }

    def hasAlignedAngles: Boolean =
      (abs(calcAngle(corners(0), corners(1)) - calcAngle(corners(3), corners(2))) < 10 &&
        abs(calcAngle(corners(0), corners(3)) - calcAngle(corners(1), corners(2))) < 10)

    hasAlignedAngles
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
    for {
      row <- range
      column <- range
    } yield
      for {
        coloredSubMat <- subMat(colorWarped, mkRect(row, column, size))
        contour <- extractContour(coloredSubMat)
        (value, quality) <- detectionMethod(contour)
      } yield {
        (row, column) -> SCell(value, quality, coloredSubMat)
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

  // FIXME duplicated
  def deIndex(pos: Pos): Int = pos._1 * ssize + pos._2


}


abstract trait SolverStrategy extends HasDescription {

  def solve(s: String)(log: String => Unit): Future[String]
}

object MockSolver extends SolverStrategy {
  val description = "mock test solver"

  def solve(s: String)(log: String => Unit): Future[String] = Future {
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
object BruteForceSolver extends SolverStrategy with Utils {

  val description = "default"

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
   * @param stringRep
   * @return
   */
  def solve(stringRep: String)(log: String => Unit): Future[String] = {
    val before = System.currentTimeMillis()

    val stringRep2 = """000000000
                       |040070300
                       |059060001
                       |006240000
                       |000050400
                       |008790000
                       |034000008
                       |000080906
                       |000400000""".stripMargin

    log(s"$before : scheduling solving ...")
    log(s"\n$stringRep")
    execFuture {
      log(s"$before : starting solving ...")
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
        case (9, yy) => search(0, y + 1, f, accu) // next row
        case (0, 9) => f(accu) // found a solution
        case (xx, yy) => if (mx(y)(x) != '0') search(x + 1, y, f, accu)
        else
          fold((accu: Int, n: Int) =>
            if (invalid(0, x, y, (n + 48).toChar)) accu
            else {
              mx(y)(x) = (n + 48).toChar
              val newaccu = search(x + 1, y, f, accu)
              mx(y)(x) = '0'
              newaccu
            }, accu, 1, 10)
      }

      // The main part of the program uses the search function to accumulate
      // the total number of solutions
      search(0, 0, i => {
        print
        i + 1
      }, 0)

      val after = System.currentTimeMillis()
      log(s"$before finished solving in ${after - before} milliseconds.")
      solution.toList.mkString("\n")
      // thats all ;-)
    }
  }

}

trait SudokuOpenCVUtils extends OpenCVUtils {

  def mkRect(pos: Pos, c: Mat): Rect = {
    val (width, height) = (c.size.width, c.size.height)
    val (x, y) = (pos._2 * width, pos._1 * height)
    new Rect(new Point(x, y), c.size)
  }

  def mkRect(row: Int, column: Int, size: Size): Rect = {
    new Rect(new Point(column * size.width, row * size.height), size)
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


object SudokuParameters {

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


case class SudokuState(cap: Int = 8,
                       minHits: Int = 0) extends Utils with CanLog {

  import SudokuParameters._

  // for every position, there is a list which depicts how often a number was found in the
  // sudoku, where the index in the list depicts the number (from 0 to 9, with 0 being the "empty" cell)
  val posFrequencies: Array[Frequency] = Array.fill[Frequency](SudokuParameters.positions.size)(Array.fill[SCount](SudokuParameters.digitRange.size)(0))

  val digitLibrary: Array[Option[Mat]] = Array.fill[Option[Mat]](SudokuParameters.digitRange.size)(None)
  val digitHitQuality: Array[Double] = Array.fill[Double](SudokuParameters.digitRange.size)(Double.MaxValue)
  private var someSolution: Option[String] = None
  initialize()

  def solve(stringRep : String) = BruteForceSolver.solve(stringRep)(logInfo)

  def initialize() = {
    posFrequencies.foreach(freq => digitRange.map(freq(_) = 0))
    for (i <- range) {
      digitLibrary(i) = None
      digitHitQuality(i) = Double.MaxValue
    }
  }

  def getDigitMat(number: Int): Option[Mat] = digitLibrary(number)

  def deIndex(pos: Pos): Int = pos._1 * ssize + pos._2

  def color(pos: Pos): Scalar = {
    val vals = posFrequencies(deIndex(pos))
    val n = vals.max.toDouble
    val s = new Scalar(0, n * 256 / cap, 256 - n * 256 / cap)
    s
  }


  def updateDigitLibrary(candidates: Cells): Unit = {
    for ((pos, cell) <- candidates if (cell.value != 0 && (digitLibrary(cell.value).isEmpty ||
      cell.quality < digitHitQuality(cell.value)))) {
      digitLibrary(cell.value) = Some(cell.data)
      digitHitQuality(cell.value) = cell.quality
    }
  }

  def setSomeSolution(s: Option[String]) = someSolution = s

  def getSomeSolution: Option[String] = someSolution

  private def sectorWellFormed(row: Int, col: Int, value: Int): Boolean = {
    val rowSector = sectors(row / 3)
    val colSector = sectors(col / 3)
    val index = deIndex((row, col))
    val sectorVals =
      for {r <- rowSector if (r != row)
           c <- colSector if (c != col)
           (count, num) <- posFrequencies(index).zipWithIndex if (count == cap)} yield num
    !sectorVals.contains(value)
  }

  private def rowColWellFormed(row: Int, col: Int, value: Int): Boolean = {
    val colVals =
      for {c <- range if (c != col &&
        posFrequencies(deIndex((row, c))).contains(value) &&
        posFrequencies(deIndex((row, c)))(value) == cap)} yield value

    val rowVals =
      for {r <- range if (r != row &&
        posFrequencies(deIndex((r, col))).contains(value) &&
        posFrequencies(deIndex((r, col)))(value) == cap)} yield value

    colVals.isEmpty && rowVals.isEmpty
  }

  def posWellFormed(pos: Pos, value: Int): Boolean = {
    val (row, col) = pos
    rowColWellFormed(row, col, value) && sectorWellFormed(row, col, value)
  }

  /**
   * updates the hit database.
   *
   * @param cells
   */
  private def updateHits(cells: Cells): Unit = {

    def updateFrequency(pos: Pos, value: Int): Unit = {
      require(0 <= value && (value <= 9), s"$value was not in interval 0 <= x <= 9 !")
      val frequencyAtPos = posFrequencies(deIndex(pos))
      if (frequencyAtPos.max < cap) {
        frequencyAtPos(value) = (1 + frequencyAtPos(value))
        ()
      }
    }

    updateDigitLibrary(cells)
    val result =
      for ((pos, SCell(value, _, _)) <- cells) yield {
        if ((value == 0) || posWellFormed(pos, value)) {
          updateFrequency(pos, value)
          true
        } else {
          false
        }
      }
    if (!result.forall(p => p)) initialize()
  }

  // search on all positions for potential hits (don't count the "empty"/"zero" fields
  def detectedNumbers: Iterable[SCount] = {
    val areWeThereyet0 =
      for {
        frequency <- posFrequencies
      } yield {
        val filtered = frequency.drop(1).filter(_ >= cap)
        if (filtered.isEmpty) 0 else filtered.max
      }

    areWeThereyet0.filter(_ != 0)
  }

  def isGoodEnough: Boolean = {
    detectedNumbers.size > minHits //&& areWeThereyet.foldLeft(true)((acc, count) => count == cap)
  }

  def mkValueMatrix: Map[Pos, SNum] = {
    (for (pos <- positions)
    yield {
      pos -> {
        (for ((v, i) <- posFrequencies(deIndex(pos)).zipWithIndex if (v == cap)) yield i).headOption.getOrElse(0)
      }
    }).toMap
  }

  def mkValueIntermediateMatrix: Map[Pos, SNum] = {
    (for (pos <- positions) yield {
      pos -> {
        (for ((v, i) <- posFrequencies(deIndex(pos)).zipWithIndex) yield i).headOption.getOrElse(0)
      }
    }).toMap
  }


  def mkValueDebugMatrix: Map[Pos, String] = {
    (for (pos <- positions) yield {
      pos -> {
        (for ((v, i) <- posFrequencies(deIndex(pos)).zipWithIndex if (v == cap)) yield i.toString).headOption.getOrElse(".")
      }
    }).toMap
  }

  def mkCountMatrix: Map[Pos, String] = {
    (for (pos <- positions) yield {
      pos -> {
        (for ((v, i) <- posFrequencies(deIndex(pos)).zipWithIndex) yield {
          if (v > cap) v.toString else s"$i[$v]"
        }).headOption.getOrElse(".")
      }
    }).toMap
  }

  /**
   * returns the the value which matches the cap for all positions in the sudoku.
   * @return
   */
  def getSomeResult: Option[Map[Pos, SNum]] = {
    if (isGoodEnough) {
      Some(mkValueMatrix)
    } else {
      None
    }
  }

  def computeSolution(cells: Cells): Future[String] = {
    updateHits(cells)
    option(getSomeResult)({
      val solverString = toSolverString(mkValueIntermediateMatrix)
      Future.successful(solverString)
    },
    result => {
      option(getSomeSolution)({
        val f = solve(toSolverString(result))
        for (s <- f if isValidSolution(s)) {
          setSomeSolution(Some(s))
        }
        f
      },
      s => Future.successful(s))
    })
  }

  def toSolverString(solution: Map[Pos, SNum]): String = {
    val res =
      (for (r <- range) yield {
        (for (c <- range) yield solution.getOrElse((r, c), "0")).foldLeft("")((a, b) => a + b) + "\n"
      }).foldLeft("")((a, b) => a + b)
    res
  }

  def isValidSolution(solvedString: String): Boolean = {
    if (!solvedString.isEmpty) {
      val normedSolverString = solvedString.replaceAll( """\n""", "").substring(0, 81)
      normedSolverString.foldLeft(0)((sum, a) => sum + a.asDigit) == 405
    } else false
  }

  /**
   * Performance:
   *
   * Benchmark                                          Mode   Samples         Mean   Mean error    Units
   * n.l.a.s.SudokuBenchmark.measureToSolutionCells     avgt        10        0.009        0.000    ms/op
   *
   * @return
   */
  // TODO move to SudokuHistory
  def toSolutionCells(solvedString: String): Future[Cells] = {
    execFuture {
      if (solvedString.isEmpty) {
        System.err.println("SolvedString was empty - not a valid solution.")
        Map()
        //require(!solvedString.isEmpty, s"solution could not be computed for \n$solvedString")
      } else {
        val normedSolverString = solvedString.replaceAll( """\n""", "").substring(0, 81)
        val allCells: Cells =
          (for (pos <- positions) yield {
            val value = normedSolverString(deIndex(pos)).asDigit
            val x: Option[((Int, Int), SCell)] =
              if (value != 0) {
                val someM = digitLibrary(value)
                (if (someM.isEmpty) {
                  digitLibrary(value) = mkFallback(value, digitLibrary)
                  digitLibrary(value)
                } else someM)
                  .map { case m => pos -> SCell(value, 0, m)}
              } else None
            x
          }).flatten.toSeq.toMap

        allCells
      }
    }
  }


  /**
   * provides a fallback if there is no digit detected for this number.
   *
   * the size and type of the mat is calculated by looking at the other elements of the digit
   * library. if none found there, just returns null
   *
   * @param number
   * @param digitLibrary
   * @return
   */
  // TODO fallback sollte eigentlich eine mask auf dem inputbild sein (maske is the best match)
  def mkFallback(number: Int, digitLibrary: Array[Option[Mat]]): Option[Mat] = {
    for ((size, matType) <- determineMatParams(digitLibrary)) yield {
      val mat = new Mat(size.height.toInt, size.width.toInt, matType).setTo(new Scalar(255, 255, 255))
      Core.putText(mat, number.toString, new Point(size.width * 0.3, size.height * 0.9), Core.FONT_HERSHEY_TRIPLEX, 2, new Scalar(0, 0, 0))
      mat
    }
  }

  /**
   * returns size and type of Mat's contained int he digitLibrary
   * @param digitLibrary
   * @return
   */
  def determineMatParams(digitLibrary: Array[Option[Mat]]): Option[(Size, Int)] = {
    digitLibrary.flatten.headOption.map { case m => (m.size, m.`type`)}
  }

  /*
def stats = "\n---------------------------------\n" +
debug(mkCountMatrix) +
"\n---------------------------------\n" +
debug(mkValueDebugMatrix) +
"\n---------------------------------\n" +
toString


def debug(cells: Map[Pos, String]): String = {
val res =
(for (r <- range) yield {
  (for (c <- range) yield {
    val res = cells((r, c))
    res
  }).foldLeft("")((a, b) => s"$a\t$b") + "\n"
}).foldLeft("")((a, b) => a + b)
res
}
*/
}