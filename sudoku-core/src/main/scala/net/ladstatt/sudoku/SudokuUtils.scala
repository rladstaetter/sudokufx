package net.ladstatt.sudoku

import net.ladstatt.core.CollectionUtils
import net.ladstatt.sudoku.JavaCV._
import net.ladstatt.sudoku.Parameters._
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core._

import scala.concurrent.duration.FiniteDuration
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
            , maxDuration: FiniteDuration): Option[SudokuHistory] = {
    BruteForceSolver.solve(solutionCandidate, maxDuration.toMillis).map(solution => SudokuHistory(solution.map(_.asDigit).toSeq, Sudoku.minNrOfValueHits))
  }

  def withCap(cap: Int)(v: Int): Boolean = v >= cap

  def mkSudokuMatrix(hitCounts: Seq[Map[Int, Int]], cap: Int): SudokuDigitSolution = mkVM(hitCounts)(withCap(cap)(_))

  def mkIntermediateSudokuMatrix(hitCounts: Seq[Map[Int, Int]]): SudokuDigitSolution = mkVM(hitCounts)(_ => true)

  def mkVM(hitCounts: Seq[Map[Int, Int]])(p: Int => Boolean): SudokuDigitSolution = {
    //hitCounts.map { case (value,frequency) => }
    val h =
      for (i <- cellRange) yield {
        //((for ((value, frequency) <- hitCounts(i) if p(frequency)) yield value).headOption.getOrElse(0) + 48).toChar
        (Random.shuffle(for ((value, frequency) <- hitCounts(i) if p(frequency)) yield value).headOption.getOrElse(0) + 48).toChar
      }
    h.toArray
  }


  def toSolutionCells(frameNr: Int
                      , digitLibrary: DigitLibrary
                      , sudokuHistory: SudokuHistory): Seq[SCell] = {
    (for (pos <- cellRange) yield {
      val value = sudokuHistory.cellValues(pos)

      val x: Option[SCell] =
        if (value != 0) {
          val someM: Option[Mat] = digitLibrary.digits(value).optMat
          (if (someM.isEmpty) {
            digitLibrary.digits(value).optMat
          } else someM)
            .map(m => SCell("fixmeid", frameNr, pos, m, new Rect, sudokuHistory.cells(pos)))
        } else None
      x
    }).flatten
  }


  /**
   * paints green borders around the cells
   *
   * @param canvas
   * @param rects
   * @param hitCounts
   * @return
   */
  def paintCorners(canvas: Mat,
                   rects: Seq[Rect],
                   hitCounts: Seq[Map[Int, Int]],
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


    CollectionUtils.traverseWithIndex(rects)((_, i) => {
      paintRect(canvas, rects(i), color(hitCounts(i), cap), 1)
    }
    )

    canvas
  }

  /**
   * provides a fallback if there is no digit detected for this number.
   *
   * the size and type of the mat is calculated by looking at the other elements of the digit
   * library. if none found there, just returns None
   *
   * @param number
   * @return
   */
  def mkFallback(number: Int, digitLibrary: DigitLibrary): Option[Mat] = {
    /**
     * returns size and type of Mat's contained in digitLibrary
     *
     * @return
     */
    def determineMatParams(): Option[(Size, Int)] = {
      digitLibrary.digits.values.flatMap(_.optMat).headOption.map(m => (m.size, m.`type`))
    }

    for ((size, matType) <- determineMatParams()) yield {
      val m = new Mat(new Scalar(new Scalar(255, 255, 255, 255)))
      val mat = new Mat(size.height, size.width, matType).setTo(m)
      opencv_imgproc.putText(mat, number.toString, new Point((size.width * 0.3).toInt, (size.height * 0.9).toInt), opencv_imgproc.FONT_HERSHEY_TRIPLEX, 2, new Scalar(0, 0, 0, 255))
      mat
    }
  }


}



