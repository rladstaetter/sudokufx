package net.ladstatt.sudoku

import net.ladstatt.core.CanLog

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}


/**
 * The following code was the first google hit for "scala sudoku solver", adapted to compile with scala 2.10
 * I hope the author doesn't mind me reusing the code.
 *
 * http://scala-programming-language.1934581.n4.nabble.com/25-lines-Sudoku-solver-in-Scala-td1987506.html
 *
 * Also don't miss the very nice essay from peter norvig on solving sudokus
 *
 * http://norvig.com/sudoku.html
 *
 * Created by lad on 26.10.14.
 */
object BruteForceSolver extends CanLog {

  val description = "default"

  def printTime(t: Long): Unit = logInfo(s"solved in $t ms.")

  /**
   * give this function sudoku in the form
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
   * and it will return the solved sudoku (without zeros)
   *
   */
  def solve(mmx: Seq[Int], maxDuration: FiniteDuration): Option[Seq[Int]] = timeR({
    val deadline = maxDuration.fromNow
    var cnt = 0
    val mx: Array[Array[Int]] = mmx.toArray.sliding(9, 9).toArray

    def isCancelled = {
      cnt = cnt + 1
      if (deadline.isOverdue()) {
        logWarn(s"CANCEL for sudoku calculation, count $cnt.)")
        true
      } else false
    }

    // The board is represented by an array of strings (arrays of chars),
    // held in a global variable mx. The program begins by reading 9 lines
    // of input to fill the board
    val solution: Array[Int] = Array.fill(Parameters.cellCount)(0)

    def populateSolution(): Unit = {
      val mxx = mx.flatten
      for ((x, i) <- mxx.zipWithIndex) {
        solution(i) = x
      }
    }

    // The test for validity is performed by looping over i=0..8 and
    // testing the row, column and 3x3 square containing the given
    // coordinate
    @tailrec
    def invalid(i: Int, x: Int, y: Int, n: Int): Boolean =
      i < 9 && (mx(y)(i) == n || mx(i)(x) == n ||
        mx(y / 3 * 3 + i / 3)(x / 3 * 3 + i % 3) == n || invalid(i + 1, x, y, n))

    // Looping over a half-closed range of consecutive integers [l..u)
    // is factored out into a higher-order function
    @tailrec
    def fold(f: (Int, Int) => Int, accu: Int, l: Int, u: Int): Int =
      if (l == u) accu else fold(f, f(accu, l), l + 1, u)

    // The search function examines each position on the board in turn,
    // trying the numbers 1..9 in each unfilled position
    // The function is itself a higher-order fold, accumulating the value
    // accu by applying the given function f to it whenever a solution m
    // is found
    def search(x: Int, y: Int, f: Int => Int, accu: Int): Int = {
      if (!isCancelled) {
        (x, y) match {
          case (9, _) => search(0, y + 1, f, accu) // next row
          case (0, 9) => f(accu) // found a solution
          case (_, _) =>
            if (mx(y)(x) != 0) {
              search(x + 1, y, f, accu)
            } else {
              fold((accu: Int, n: Int) =>
                if (invalid(0, x, y, n)) {
                  accu
                } else {
                  mx(y)(x) = n
                  val newaccu = search(x + 1, y, f, accu)
                  mx(y)(x) = 0
                  newaccu
                }, accu, 1, 10)
            }
        }
      } else {
        throw new RuntimeException("Computation was cancelled.")
      }
    }

    // The main part of the program uses the search function to accumulate
    // the total number of solutions
    Try {
      search(0, 0, i => {
        populateSolution()
        i + 1
      }, 0)
      solution
    } match {
      case Success(s) =>
        val digits = s.toSeq
        if (s.length == 81 && 405 == digits.sum.toLong)
          Some(s.toIndexedSeq)
        else {
          logWarn("Found solution, but is invalid.")
          None
        }
      case Failure(e) =>
        logError(e.getMessage)
        None
    }
  }, "solve")

}






