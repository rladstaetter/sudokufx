package net.ladstatt.sudoku

import net.ladstatt.core.CollectionUtils
import JavaCV._
import net.ladstatt.sudoku.Parameters._
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core._

import scala.util.Random


/**
 * Contains most of the algorithms necessary for the SudokuFX application.
 */
object SudokuUtils {

  def solve(solutionCandidate: SudokuDigitSolution, maxDuration: Long): Option[SudokuDigitSolution] =
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
      println(freq4Index)
      val n = freq4Index.values.max.toDouble
      println("n:" + n + "cap: " + cap)
      // n % cap
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
    }
    else hits
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
   * Awaits a preprocessed video frame and finds the corners of the biggest rectangle seen
   * in the input.
   *
   * @return detected contours
   */
  def detectRectangle(corners1: Mat, contours: MatVector, ratio: Int): Option[Mat] = {
    val (contourArea, c) = extractCurveWithMaxArea(contours)
    val minimumExpectedArea: Double = opencv_imgproc.contourArea(corners1) / ratio
    if (contourArea > minimumExpectedArea) {
      val approxCurve: Mat = mkApproximation(c)
      if (has4Sides(approxCurve)) {
        Option(approxCurve)
        /*
                val corners = mkSortedCorners(approxCurve)
                if (isSomewhatSquare(corners)) {
                  Option(new MatOfPoint2f(corners: _*))
                } else {
                  logTrace(s"Detected ${approxCurve.size} shape, but it doesn't look like a rectangle.")
                  None
                }

         */
      } else {
        logTrace(s"Detected only ${approxCurve.size} shape, but need 1x4!")
        None
      }
    } else {
      logTrace(s"The detected area of interest was too small ($contourArea < $minimumExpectedArea).")
      None
    }

  }

  /**
   * Returns coordinates of the biggest rectangle found in input data.
   *
   * @param fp            framepipeline containing input data
   * @param contourParams parameters to configure algorithm
   * @return
   */
  def detectBiggestRectangle(fp: FramePipeline, contourParams: ContourParams): Option[Mat] = {
    val contours = JavaCV.findContours(fp.dilated, contourParams.contourMode, contourParams.contourMethod)
    detectRectangle(fp.corners, contours, contourParams.contourRatio)
  }

}



