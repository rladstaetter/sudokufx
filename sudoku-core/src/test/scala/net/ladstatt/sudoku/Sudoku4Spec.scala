package net.ladstatt.sudoku

import net.ladstatt.opencv.OpenCV
import org.opencv.imgcodecs.Imgcodecs
import org.scalatest.wordspec.AnyWordSpecLike

/**
 * Created by lad on 07.01.16.
 */
class Sudoku4Spec extends AnyWordSpecLike {

  OpenCV.loadNativeLib()

  "testSudoku4" ignore {
    val sudoku4 = Imgcodecs.imread("src/test/resources/net/ladstatt/sudoku/sudoku_4.png")
    val (_, (r, _)) = SudokuTestContext.calculate(sudoku4)
    r match {
      case s: SSuccess if s.someSolution.isDefined => Imgcodecs.imwrite("target/dest.png", s.someSolution.get.solutionMat)
      case s: SSuccess if s.someSolution.isEmpty =>
        fail("Only found corners")
      //Imgcodecs.imwrite("target/dest.png", s.solutionMat)
      case failure: SFailure => fail(failure.msg)
    }
  }
}
