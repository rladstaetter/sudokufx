package net.ladstatt.apps.sudoku


import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.GeneratorDrivenPropertyChecks

@RunWith(classOf[JUnitRunner])
final class SudokuStateTest extends FunSuite with GeneratorDrivenPropertyChecks with OpenCvUnitTest {

  // case class SudokuState(nr : Int, frame : Mat, cap : Int, minHits : Int)

  // val frameGen : Gen[Mat] =
  val stateGen: Gen[Sudoku] =
    for {nr <- Gen.choose(0, 10000)
         f <- Gen.const(frame69)
         cap <- Gen.choose(8, 15)
         minHits <- Gen.choose(20, 30)} yield Sudoku(nr, f, cap, minHits)

  def printState(s: Sudoku): Unit =
    println(s.sudokuCorners.toList)

  test("t2") {

    forAll(stateGen)(printState)

    //  forAll { (sudokuState <- pos: Int, col: Int) =>
    //    (pos > 0 && pos < 81) && (col >= 0 && col < 9) ==> assert((a + b).startsWith(a))
    //  }
  }


}
