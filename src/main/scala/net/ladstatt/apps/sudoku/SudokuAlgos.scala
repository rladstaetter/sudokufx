package net.ladstatt.apps.sudoku

import net.ladstatt.core.{HasDescription, Utils}
import org.opencv.core._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
 * Created by lad on 26.10.14.
 */
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
    def solve(mmx: SudokuDigitSolution, maxDuration: Long = 100l): Option[SudokuDigitSolution] = time({
      val before = System.currentTimeMillis()
      var cnt = 0
      val mx: Array[Array[Char]] = mmx.sliding(9, 9).toArray
      def isCancelled = {
        cnt = cnt + 1
        val duration = (System.currentTimeMillis() - before)
        if (duration > maxDuration) {
          System.err.println(s"---> CANCEL (timeout: $duration ms, count $cnt.)")
          true
        } else false
      }

      def copy(s: SudokuDigitSolution): SudokuDigitSolution = {
        s.clone()
      }

      // The board is represented by an array of strings (arrays of chars),
      // held in a global variable mx. The program begins by reading 9 lines
      // of input to fill the board
      val solution: SudokuDigitSolution = Array.fill(Parameters.cellCount)('A')

      def populateSolution() = {
        val mxx = mx.flatten
        for ((x, i) <- mxx.zipWithIndex) {
          solution(i) = x
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





  def detectCell(cell: Mat, digitQuality: Array[Double]): Future[SCell] = {
    import net.ladstatt.apps.sudoku.TemplateDetectionStrategy.detectNumber
    for {
      (value, quality) <- findCellContour(cell).map(detectNumber(_).filter(nq => nq._1 != 0 && nq._2 < digitQuality(nq._1))).getOrElse(Future.successful((0, 0.0)))
    } yield {
      SCell(value, quality, cell)
    }
  }




  def imageIOChain(input: Mat): Future[ImageIOChain] = {
    val imageIoChain = ioc(input)(Seq(duplicate _, toGray _, blur _, adaptiveThreshold _))
    val working = copySrcToDestWithMask(input, new Mat, input)
    val grayed = toGray(working)
    val blurred = blur(grayed)
    val thresholdApplied = adaptiveThreshold(blurred)
    for {
      inverted <- bitwiseNotF(imageIoChain)
      dilated <- dilate(inverted)
      eroded <- erode(inverted)
    //  dilated <- dilate(thresholdApplied)
    //  inverted <- bitwiseNot(dilated)
    } yield ImageIOChain(working, grayed, blurred, thresholdApplied, inverted, dilated, eroded)
  }

}
