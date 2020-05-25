package net.ladstatt.sudoku

import net.ladstatt.core.CanLog
import org.bytedeco.opencv.opencv_core.Mat

object SudokuState {

  import Parameters._

  private val defaultDigitLibrary: DigitLibrary = Map().withDefaultValue((Double.MaxValue, None))
  private val defaultHitCounters: HitCounters = Map().withDefaultValue(Map[Int, Int]().withDefaultValue(0))

  val DefaultState: SudokuState =
    SudokuState(defaultHitCounters
      , defaultDigitLibrary
      , cap
      , minHits
      , maxSolvingDuration)

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
                       someCells: Option[Cells] = None) extends CanLog {

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

  val detections: Int = hitCounts.values.flatMap(filterHits(_, cap)).size

  def merge(sRectangle: SRectangle): SudokuState = {
    merge(sRectangle.normalized, sRectangle.cells, sRectangle.cellValues)
  }

  def merge(normalized: Mat,
            detectedCells: Seq[SCell],
            detectedCellValues: Seq[Int]): SudokuState = {
    copy(
      library = SudokuUtils.mergeDigitLibrary(normalized, library, detectedCells),
      hitCounts = SudokuUtils.mergeHits(hitCounts, detectedCellValues))
  }

  def solve(): SudokuState = {
    if (detections >= minHits) {
      logInfo("NrDetections: " + detections + " minHits: " + minHits)
      val sudoku2Solve: SudokuDigitSolution = SudokuUtils.mkSudokuMatrix(hitCounts, cap)
      val someResult: Option[SudokuDigitSolution] = SudokuUtils.solve(sudoku2Solve, maxSolvingDuration)
      val someCells: Option[Cells] = someResult.map(SudokuUtils.toSolutionCells(library, _))
      someResult.map(_ => copy(someResult = someResult, someCells = someCells)).getOrElse({
        logInfo("Resetting to DefaultState.")
        SudokuState.DefaultState
      })
    } else this
  }


}