package net.ladstatt.sudoku

import net.ladstatt.opencv.OpenCV._
import org.opencv.core.Mat


object SudokuState {

  import Parameters._

  private val defaultDigitLibrary: DigitLibrary = Map().withDefaultValue((Double.MaxValue, None))
  private val defaultHitCounters: HitCounters = Map().withDefaultValue(Map[Int, Int]().withDefaultValue(0))

  val DefaultState = SudokuState(defaultHitCounters, defaultDigitLibrary, cap, minHits, maxSolvingDuration)

  def apply(): SudokuState = DefaultState
}

/**
  *
  * @param hitCounts
  * @param library
  * @param cap                number of detections for a certain number until it is regarded as "stable enough"
  * @param minHits            minimal number of numbers before a the solving is attempted
  * @param maxSolvingDuration number of milliseconds which the solver is given before he gives up
  *
  */
case class SudokuState(hitCounts: HitCounters,
                       library: DigitLibrary,
                       cap: Int, // TODO NOT used?
                       minHits: Int,
                       maxSolvingDuration: Long,
                       someResult: Option[SudokuDigitSolution] = None,
                       someCells: Option[Cells] = None
                      ) {

  import SudokuUtils._

  /**
    * given a frequency table, returns a number which exceed a certain threshold randomly
    *
    * @param freqs
    * @param threshold
    * @return
    */
  def filterHits(freqs: Map[Int, Int], threshold: Int): Option[(Int, Int)] = {
    freqs.find { case (value, f) => value != 0 && f >= threshold }
  }

  val detections: SCount = hitCounts.values.flatMap(filterHits(_, cap)).size

  def merge(normalized: Mat,
            detectedCells: Seq[SCell],
            detectedCellValues: Seq[SCount]): SudokuState = {
    copy(
      library = SudokuUtils.mergeDigitLibrary(normalized, library, detectedCells),
      hitCounts = SudokuUtils.mergeHits(hitCounts, detectedCellValues))
  }

  def solve(): SudokuState = {
    if (detections >= minHits) {
      logInfo("NrDetections: " + detections + " minHits: " + minHits)
      val sudoku2Solve: SudokuDigitSolution = mkSudokuMatrix(hitCounts, cap)
      val someResult: Option[SudokuDigitSolution] = SudokuUtils.solve(sudoku2Solve, maxSolvingDuration)
      val someCells: Option[Cells] = someResult.map(toSolutionCells(library, _))
      someResult.map(_ => copy(someResult = someResult, someCells = someCells)).getOrElse({
        logInfo("Resetting to DefaultState.")
        SudokuState.DefaultState
      })
    } else this
  }


}

object Parameters {

  /**
    * the maximum time the algorithm should search for a solution
    */
  val maxSolvingDuration: Long = 5000L

  // least number of matches necessary to identify one number
  // if you have a good camera, take 1 to get fast response
  val cap = 10

  // number of different values a cell can have before the cell is label 'ambiguous'
  val ambiguitiesCount = 5

  // how many cells are allowed to have ambiguous information before number detection process is restarted
  val ambiCount = 5

  // numbers won't get any larger in the status matrix than this number
  val topCap = 15


  assert(topCap - cap > 0)

  val minHits = 22

  val ssize = 9
  val cellCount = ssize * ssize

  val range = 0 until ssize
  val digitRange = 0 to ssize

  val cellRange: Range = 0 until cellCount

  val colorRange = 0 to 256 by 16
  private val leftRange: Seq[Int] = Seq(0, 1, 2)
  private val middleRange: Seq[Int] = Seq(3, 4, 5)
  private val rightRange: Seq[Int] = Seq(6, 7, 8)
  val sectors: Seq[Seq[Int]] = Seq(leftRange, middleRange, rightRange)


  def row(i: Int): Int = i / 9

  def col(i: Int): Int = i % 9

}
