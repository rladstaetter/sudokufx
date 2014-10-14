package net.ladstatt.apps.sudoku


import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV._
import org.junit.runner.RunWith
import org.opencv.core.Mat
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.junit.JUnitRunner
import org.scalacheck.Gen

@RunWith(classOf[JUnitRunner])
final class SudokuStateTest extends FunSuite with GeneratorDrivenPropertyChecks with OpenCvUnitTest {

 // case class SudokuState(nr : Int, frame : Mat, cap : Int, minHits : Int)

  val stateGen: Gen[SudokuState] =
    for {nr <- Gen.choose(0, 10000)
         f <- Gen.const(frame)
         cap <- Gen.choose(8, 15)
         minHits <- Gen.choose(20, 30)} yield  SudokuState(nr, f, cap, minHits)

  test("t2") {

    forAll(stateGen) { (s: SudokuState) => {
      println(s.detectedCorners.toList)
    }}

    //  forAll { (sudokuState <- pos: Int, col: Int) =>
    //    (pos > 0 && pos < 81) && (col >= 0 && col < 9) ==> assert((a + b).startsWith(a))
    //  }
  }


}
