package net.ladstatt.sudoku

import org.scalatest.wordspec.AnyWordSpecLike


class SolverSpec extends AnyWordSpecLike {

  val solved =
    """483921657
      |967345821
      |251876493
      |548132976
      |729564138
      |136798245
      |372689514
      |814253769
      |695417382""".stripMargin.replaceAll("\n", "")

  def solveReadableSudoku(sudokuWithNewLines: String): Option[SudokuDigitSolution] = {
    BruteForceSolver.solve(sudokuWithNewLines.replaceAll("\n", "").toCharArray, 5000L)
  }

  "testSum" in {
    assert(405 == solved.toArray.map(_.asDigit).sum.toLong)
  }


  "solve sudoku 1" in {
    val detectedNumbers =
      """608001020
        |009302580
        |000890300
        |000200090
        |300080007
        |040006000
        |003025000
        |094103200
        |070600903
        |""".stripMargin
    assert(solveReadableSudoku(detectedNumbers).isDefined)
  }
/*
  "testSolving" in {
    Try {
      val solvedSudokus: Array[Option[SudokuDigitSolution]] =
        for (sudokuAsString <- SudokuTestContext.easySudokus.split("========"))
          yield solveReadableSudoku(sudokuAsString)

      for (fs <- solvedSudokus.flatten) {
        assert(405 == fs.map(_.asDigit).sum.toLong)
      }
    } match {
      case Success(_) =>
      case Failure(e) => fail(e.getMessage)
    }

  }
*/
  /**
   * Test shows what happens if we try to solve an malformed input
   */
  "SolveWrongInput" in {
    val sudokuInput =
      """003020601
        |900305001
        |001806400
        |008102900
        |700000008
        |006708200
        |002609500
        |800203009
        |005010300""".stripMargin

    solveReadableSudoku(sudokuInput) match {
      case Some(s) => assert(405 != s.map(_.asDigit).sum.toLong)
      case None =>
    }
  }


}
