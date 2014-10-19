package net.ladstatt.apps.sudoku


import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.GeneratorDrivenPropertyChecks

@RunWith(classOf[JUnitRunner])
final class SudokuStateTest extends FunSuite with GeneratorDrivenPropertyChecks with OpenCvUnitTest {

  // case class SudokuState(nr : Int, frame : Mat, cap : Int, minHits : Int)

  val stateGen: Gen[SudokuState] =
    for {nr <- Gen.choose(0, 10000)
         f <- Gen.const(frame)
         cap <- Gen.choose(8, 15)
         minHits <- Gen.choose(20, 30)} yield SudokuState(nr, f, cap, minHits)

  test("t2") {

    forAll(stateGen) { (s: SudokuState) => {
      println(s.detectedCorners.toList)
    }
    }

    //  forAll { (sudokuState <- pos: Int, col: Int) =>
    //    (pos > 0 && pos < 81) && (col >= 0 && col < 9) ==> assert((a + b).startsWith(a))
    //  }
  }


}
