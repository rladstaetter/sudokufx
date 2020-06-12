package net.ladstatt.sudoku

import net.ladstatt.core.CollectionUtils
import net.ladstatt.sudoku.JavaCV._
import net.ladstatt.sudoku.Parameters._
import org.bytedeco.javacpp.FloatPointer
import org.bytedeco.opencv.global.{opencv_core, opencv_imgproc}
import org.bytedeco.opencv.opencv_core._

import scala.util.Random

case class PFloat(x: Float, y: Float) {
  val dist = Math.sqrt(x * x + y * y)
}

case class PInt(x: Int, y: Int) {
  val dist = Math.sqrt(x * x + y * y)
}

/**
 * Contains most of the algorithms necessary for the SudokuFX application.
 */
object SudokuUtils {

  def solve(solutionCandidate: SudokuDigitSolution
            , maxDuration: Long): Option[SudokuDigitSolution] =
    BruteForceSolver.solve(solutionCandidate, maxDuration)

  def withCap(cap: Int)(v: Int): Boolean = v >= cap

  def mkSudokuMatrix(hitCounts: HitCounters, cap: Int): SudokuDigitSolution = mkVM(hitCounts)(withCap(cap)(_))

  def mkIntermediateSudokuMatrix(hitCounts: HitCounters): SudokuDigitSolution = mkVM(hitCounts)(_ => true)

  def mkVM(hitCounts: HitCounters)(p: Int => Boolean): SudokuDigitSolution = {
    //hitCounts.map { case (value,frequency) => }
    val h =
      for (i <- cellRange) yield {
        //((for ((value, frequency) <- hitCounts(i) if p(frequency)) yield value).headOption.getOrElse(0) + 48).toChar
        (Random.shuffle(for ((value, frequency) <- hitCounts(i) if p(frequency)) yield value).headOption.getOrElse(0) + 48).toChar
      }
    h.toArray
  }


  def toSolutionCells(digitLibrary: DigitLibrary, digitSolution: SudokuDigitSolution): Cells = {
    val allCells: Cells =
      (for (pos <- cellRange) yield {
        val value = digitSolution(pos).asDigit

        val x: Option[SCell] =
          if (value != 0) {
            val someM: Option[Mat] = digitLibrary(value)._2
            (if (someM.isEmpty) {
              //              digitData(value) = mkFallback(value, digitData)
              digitLibrary(value)._2
            } else someM)
              .map(m => SCell(m, new Rect))
          } else None
        x
      }).flatten.toArray

    allCells
  }


  /**
   * paints green borders around the cells
   *
   * @param canvas
   * @param rects
   * @param someSolution
   * @param hitCounts
   * @return
   */
  def paintCorners(canvas: Mat,
                   rects: Seq[Rect],
                   someSolution: Option[Cells],
                   hitCounts: HitCounters,
                   cap: Int): Mat = {


    // TODO update colors
    def color(freq4Index: Map[Int, Int], cap: Int): Scalar = {
      val n = freq4Index.values.max.toDouble
      val r = freq4Index.size match {
        case 1 => 0
        case 2 => 100
        case 3 => 200
        case _ => 255
      }
      new Scalar(0, (n % cap) * 255 / cap, r.toDouble, 255.0)
    }


    for (_ <- someSolution) {
      CollectionUtils.traverseWithIndex(rects)((_, i) => {
        paintRect(canvas, rects(i), color(hitCounts(i), cap), 1)
      }
      )
    }

    canvas
  }

  /**
   * provides a fallback if there is no digit detected for this number.
   *
   * the size and type of the mat is calculated by looking at the other elements of the digit
   * library. if none found there, just returns null
   *
   * @param number
   * @return
   */
  def mkFallback(number: Int, digitLibrary: DigitLibrary): Option[Mat] = {
    /**
     * returns size and type of Mat's contained int he digitLibrary
     *
     * @return
     */
    def determineMatParams(): Option[(Size, Int)] = {
      digitLibrary.values.flatMap(_._2).headOption.map(m => (m.size, m.`type`))
    }

    for ((size, matType) <- determineMatParams()) yield {
      val m = new Mat(new Scalar(new Scalar(255, 255, 255, 255)))
      val mat = new Mat(size.height, size.width, matType).setTo(m)
      opencv_imgproc.putText(mat, number.toString, new Point((size.width * 0.3).toInt, (size.height * 0.9).toInt), opencv_imgproc.FONT_HERSHEY_TRIPLEX, 2, new Scalar(0, 0, 0, 255))
      mat
    }
  }

  def mergeHits(currentHitCounts: HitCounters, detections: Seq[Int]): HitCounters = {
    val hits =
      (for ((value, index) <- detections.zipWithIndex) yield {
        val frequencies: Map[Int, Int] = currentHitCounts(index)
        index -> (frequencies + (value -> (frequencies(value) + 1)))
      }).toMap
    val cellAmbiguities = hits.values.map(m => m.size).count(_ > Parameters.ambiguitiesCount)
    if (cellAmbiguities > Parameters.ambiCount) {
      logError(s"Too many ambiguities ($cellAmbiguities), resetting .. ")
      SudokuState.DefaultState.hitCounts
    } else {
      hits
    }
  }


  def mergeDigitLibrary(sudokuCanvas: Mat,
                        digitLibrary: DigitLibrary,
                        detectedCells: Seq[SCell]): DigitLibrary = {

    /**
     * The filter returns only cells which contain 'better match' cells.
     *
     * If there are cells containing '0' detected they are ignored.
     */
    val qualityFilter: PartialFunction[SCell, Boolean] = {
      case c => (c.value != 0) && (c.quality < digitLibrary(c.value)._1) // lower means "better"
    }

    val hits: Seq[SCell] = detectedCells.filter(qualityFilter)
    val grouped: Map[Int, Seq[SCell]] = hits.groupBy(f => f.value)
    //    import Ordering.Double.TotalOrdering
    val optimal: Map[Int, SCell] = grouped.map { case (i, cells) => i -> cells.maxBy(c => c.quality)(Ordering.Double.TotalOrdering) }

    digitLibrary ++
      (for (c <- optimal.values if digitLibrary(c.value)._1 > c.quality) yield {
        val newData = Some(copyMat(sudokuCanvas.apply(c.roi)))
        c.value -> ((c.quality, newData))
      }).toMap
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
    val ps = JavaCV.extractIntPoints(m)

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

    val fp = new FloatPointer(A.x, A.y
      , B.x, B.y
      , C.x, C.y
      , D.x, D.y
    )
    new Mat(new Size(2, 4), opencv_core.CV_32F, fp)
  }

  /**
   * Awaits a preprocessed video frame and finds the corners of the biggest rectangle seen
   * in the input.
   *
   * @return detected contours
   */
  def detectRectangle(contours: MatVector, minimumExpectedArea: Double): Option[Mat] = {
    val (contourArea, curve) = extractCurveWithMaxArea(contours)
    if (contourArea > minimumExpectedArea) {
      Option(FramePipeline.approximate(curve)).filter(has4Sides).map(m => sortCorners(m))
    } else {
      logTrace(s"The detected area of interest was too small ($contourArea < $minimumExpectedArea).")
      None
    }
  }

  /**
   * Returns coordinates of the biggest rectangle found in input data.
   */
  def detectSudokuCanvas(sudoku: Sudoku): Option[SudokuCanvas] = {
    detectSudokuCanvas(sudoku.contourParams)(sudoku.pipeline)
  }

  def detectSudokuCanvas(contourParams: ContourParams)(fp: FramePipeline): Option[SudokuCanvas] = {
    val contours: MatVector = JavaCV.findContours(fp.dilated, contourParams.retrivalMode, contourParams.approximation)
    val inputFrameCorners = JavaCV.mkCorners(fp.frame)
    val minimumExpectedArea: Double = opencv_imgproc.contourArea(inputFrameCorners) / contourParams.contourRatio
    detectRectangle(contours, minimumExpectedArea).map(detectedCorners => SudokuCanvas(fp.frame, detectedCorners))
  }

}



