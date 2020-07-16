package net.ladstatt.sudoku

import org.scalatest.wordspec.AnyWordSpecLike

class SudokuConfigurationSpec extends AnyWordSpecLike {

  "UnsolvedExact" should {
    /** just per se s1ReadyToSolve is not eligible for starting solution. */
    "without history is not ready to solve" in assert(Sudokus.s1ReadyToSolve.isReadyToSolve)
    "check solving" in {
      val config = Sudokus.s1ReadyToSolve.assumeReadyToSolve
      assert(!config.isSolved)
      assert(config.isReadyToSolve)
      val solved: SudokuState = config.solved
      assert(solved.isSolved)
      assert(solved != config)
      println(solved.asSudokuString)
    }

  }
}
