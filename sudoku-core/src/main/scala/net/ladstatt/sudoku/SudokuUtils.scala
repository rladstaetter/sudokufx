package net.ladstatt.sudoku

import java.io.File

import net.ladstatt.sudoku.Parameters._
import net.ladstatt.core.CollectionUtils
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._
import org.opencv.imgproc.Imgproc

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Try, Random}


/**
  * Contains most of the algorithms necessary for the Sudoku Capturer application.
  */
object SudokuUtils {

  /**
   * given a frequency table, returns a number which exceed a certain threshold randomly
   *
   * @param freqs
   * @param threshold
   * @return
   */
  def filterHits(freqs: Map[Int, Int], threshold: Int): Option[(Int, Int)] = {
    freqs.find { case (value, f) => value != 0 && f >= threshold}
  }

  def nrDetections(hitCounts: HitCounters, cap: Int): Int = {
    hitCounts.values.flatMap(filterHits(_, cap)).size
  }


  def computeSolution(hitCounters: HitCounters,
                      digitLibrary: DigitLibrary,
                      cap: Int,
                      minHits: Int,
                      maxDuration: Long): Future[(Option[SudokuDigitSolution], Option[Cells], HitCounters, DigitLibrary)] =
    Future {
      val (someDigitSolution, currentHits, currentDigitLibrary) =
        if (nrDetections(hitCounters, cap) >= minHits) {
          logInfo("Trying to solve with detectednumbers: " + nrDetections(hitCounters, cap) + ", minHits: " + minHits)
          val sudoku2Solve: SudokuDigitSolution = mkSudokuMatrix(hitCounters, cap)
          val someResult: Option[SudokuDigitSolution] = solve(sudoku2Solve, maxDuration)
          someResult.foreach {
            case a =>
              println(a.sliding(9, 9).map(new String(_)).mkString("\n"))
          }
          (someResult,
            if (someResult.isDefined) hitCounters else Parameters.defaultHitCounters,
            if (someResult.isDefined) digitLibrary else Parameters.defaultDigitLibrary) // reset if no valid solution was found
        }
        else
        //  (Some(mkIntermediateSudokuMatrix(hitCounters)), hitCounters, digitLibrary)
          (None, hitCounters, digitLibrary)

      val someCells: Option[Cells] = someDigitSolution.map(toSolutionCells(digitLibrary, _))
      (someDigitSolution, someCells, currentHits, currentDigitLibrary)
    }

  private def solve(solutionCandidate: SudokuDigitSolution, maxDuration: Long): Option[SudokuDigitSolution] = BruteForceSolver.solve(solutionCandidate, maxDuration)

  def withCap(cap: Int)(v: Int) = v >= cap

  def mkSudokuMatrix(hitCounts: HitCounters, cap: Int): SudokuDigitSolution = mkVM(hitCounts)(withCap(cap)(_))

  def mkIntermediateSudokuMatrix(hitCounts: HitCounters): SudokuDigitSolution = mkVM(hitCounts)(_ => true)

  def mkVM(hitCounts: HitCounters)(p: Int => Boolean): SudokuDigitSolution = {
    val h =
      for (i <- cellRange) yield {
        (Random.shuffle(for ((value, frequency) <- hitCounts(i) if p(frequency)) yield value).headOption.getOrElse(0) + 48).toChar
      }
    h.toArray
  }


  /**
   * Performance:
   *
   * Benchmark                                          Mode   Samples         Mean   Mean error    Units
   * n.l.a.s.SudokuBenchmark.measureToSolutionCells     avgt        10        0.009        0.000    ms/op
   *
   * @return
   */
  def toSolutionCells(digitLibrary: DigitLibrary, digitSolution: SudokuDigitSolution): Cells = {
    val allCells: Cells =
      (for (pos <- cellRange) yield {
        val value = digitSolution(pos).asDigit

        val x: Option[SCell] =
          if (value != 0) {
            val someM = digitLibrary(value)._2
            (if (someM.isEmpty) {
              //              digitData(value) = mkFallback(value, digitData)
              digitLibrary(value)._2
            } else someM)
              .map(m => SCell(value, 0, new Rect))
          } else None
        x
      }).flatten.toArray

    allCells
  }


  /**
   * paints green borders around the cells
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
                   cap: Int): Future[Mat] = {


    // TODO update colors
    def color(hitCounts: HitCounters, i: Int, cap: Int): Scalar = {
      val freq4Index = hitCounts(i)
      val n = freq4Index.values.max.toDouble
      val s = new Scalar(0, n * 256 / cap, 256 - n * 256 / cap)
      s
    }

    Future {
      for (solution <- someSolution) {
        CollectionUtils.traverseWithIndex(rects)((cell, i) => {
          paintRect(canvas, rects(i), color(hitCounts, i, cap), 1)
        }
        )
      }

      canvas
    }
  }


  /**
    * paints the solution to the canvas.
    *
    * returns the modified canvas with the solution painted upon.
    *
    * detectedCells contains values from 0 to 9, with 0 being the cells which are 'empty' and thus have to be filled up
    * with numbers.
    *
    * uses digitData as lookup table to paint onto the canvas, thus modifying the canvas.
    */
  def paintSolution(canvas: Mat,
                    detectedCells: Seq[Int],
                    someSolution: Option[Cells],
                    digitLibrary: DigitLibrary,
                    rects: Seq[Rect]): Future[Mat] = {

    Future {
      for (solution <- someSolution) {
        val values = solution.map(_.value)
        for ((s, r) <- values zip rects if values.sum == 405) {
          copyTo(digitLibrary(s)._2.getOrElse(mkFallback(s, digitLibrary).get), canvas, r)
        }
      }
      canvas
    }
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
  private def mkFallback(number: Int, digitLibrary: DigitLibrary): Option[Mat] = {
    /**
      * returns size and type of Mat's contained int he digitLibrary
      * @return
      */
    def determineMatParams(): Option[(Size, Int)] = {
      digitLibrary.values.flatMap(_._2).headOption.map {
        case m => (m.size, m.`type`)
      }
    }

    for ((size, matType) <- determineMatParams()) yield {
      val mat = new Mat(size.height.toInt, size.width.toInt, matType).setTo(new Scalar(255, 255, 255))
      Imgproc.putText(mat, number.toString, new Point(size.width * 0.3, size.height * 0.9), Core.FONT_HERSHEY_TRIPLEX, 2, new Scalar(0, 0, 0))
      mat
    }
  }

  def mergeHits(currentHitCounts: HitCounters, detections: Seq[Int], cap: Int): HitCounters = {
    val hits =
      (for ((value, index) <- detections.zipWithIndex) yield {
        val frequencies: Map[Int, Int] = currentHitCounts(index)
        index -> (frequencies + (value -> (frequencies(value) + 1)))
      }).toMap

    SudokuUtils.resetHitsIfThereAreTooMuchAmbiguities(hits)
  }


  def resetHitsIfThereAreTooMuchAmbiguities(counters: HitCounters): HitCounters = {
    val cellAmbiguities = counters.values.map(m => m.size).count(_ > Parameters.ambiguitiesCount)
    if (cellAmbiguities > Parameters.ambiCount) {
      logError(s"Too many ambiguities ($cellAmbiguities), resetting .. ")
      Parameters.defaultHitCounters
    }
    else counters
  }

  // TODO add some sort of normalisation for each cell with such an effect that every cell has the same color 'tone'
  // TODO remove sudokuCanvas from signature: just save roi's and calculate Mats on demand
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
    val optimal: Map[Int, SCell] = grouped.map { case (i, cells) => i -> cells.maxBy(c => c.quality) }

    digitLibrary ++
      (for (c <- optimal.values if digitLibrary(c.value)._1 > c.quality) yield {
        val newData = Some(copyMat(sudokuCanvas.submat(c.roi)))
        c.value -> ((c.quality, newData))
      }).toMap
  }

  def persist(file: File): Try[File] = {
    Try(file)
    //    OpenCV.persist(frame, file)
  }


  def detectSudokuCorners(input: Mat, ratio: Int = 30): MatOfPoint2f = {
    import scala.collection.JavaConversions._
    extractCurveWithMaxArea(coreFindContours(input)) match {
      case None =>
        logWarn("Could not detect any curve ... ")
        EmptyCorners
      case Some((maxArea, c)) =>
        val expectedMaxArea = Imgproc.contourArea(mkCorners(input.size)) / ratio
        if (maxArea > expectedMaxArea) {
          val approxCurve = mkApproximation(new MatOfPoint2f(c.toList: _*))
          if (has4Sides(approxCurve)) {
            val corners = mkSortedCorners(approxCurve)
            if (isSomewhatSquare(corners.toList)) {
              corners
            } else {
              logTrace(s"Detected ${approxCurve.size} shape, but it doesn't look like a sudoku!")
              EmptyCorners
            }
          } else {
            logTrace(s"Detected only ${approxCurve.size} shape, but need 1x4!")
            EmptyCorners
          }
        } else {
          logTrace(s"The detected area of interest was too small ($maxArea < $expectedMaxArea).")
          EmptyCorners
        }
    }

  }

}



