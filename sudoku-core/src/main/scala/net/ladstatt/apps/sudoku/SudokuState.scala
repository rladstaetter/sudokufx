package net.ladstatt.apps.sudoku

import net.ladstatt.apps.sudoku.Parameters._
import net.ladstatt.core.CollectionUtils
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random


/*
object NewCandidate {

 def sectorIndizes(i: Int): Set[Int] = {
   val rowSector: Seq[Int] = sectors(row(i) / 3)
   val colSector: Seq[Int] = sectors(col(i) / 3)
   (for {r <- rowSector
         c <- colSector} yield r * 9 + c).toSet -- Set(i)
 }

 def isValid(hitCounts: HitCounters, values: Seq[Int], cap: Int): Boolean =
   values.zipWithIndex.forall {
     case (c, i) => posWellFormed(hitCounts, i, c, cap)
   }

 // searches rows and cols if there exist already the value in the same row or column
 private def rowColWellFormed(hitCounts: HitCounters, i: Int, value: Int, cap: Int): Boolean = {
   // val otherCells = cellRange.filter(u => u != i && ((row(u) == row(i) || col(u) == col(i)) || sectorIndizes(i).contains(u)))
   val otherCells = cellRange.filter(u => u != i && (row(u) == row(i) || col(u) == col(i)))
   !otherCells.exists(i => hitCounts(i).contains(value) && hitCounts(i)(value) == cap)
   // !otherCells.exists(i => hitCounts(i).contains(value))
 }

 def posWellFormed(hitCounts: HitCounters, i: SIndex, value: Int, cap: Int): Boolean = {
   value == 0 || rowColWellFormed(hitCounts, i, value, cap) //&& sectorWellFormed(hitCounts, i, value)
 }


}
    */

case class SudokuState(cells: Seq[SCell] = Seq()) {

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
          val someResult = solve(sudoku2Solve, maxDuration)
          (someResult,
            if (someResult.isDefined) hitCounters else Parameters.defaultHitCounts,
            if (someResult.isDefined) digitLibrary else Parameters.defaultLibrary) // reset if no valid solution was found
        }
        else
        //  (Some(mkIntermediateSudokuMatrix(hitCounters)), hitCounters, digitLibrary)
          (None, hitCounters, digitLibrary)

      val someCells = someDigitSolution.map(toSolutionCells(digitLibrary, _))
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
        CollectionUtils.traverseWithIndex(rects)((cell, i) =>
          paintRect(canvas, rects(i), color(hitCounts, i, cap), 1)
        )
      }

      canvas
    }
  }

}



