package net.ladstatt.apps.sudoku

import net.ladstatt.core.Utils
import org.junit.Assert._
import org.junit.Test

import scala.io.Source
import scala.util.{Failure, Success, Try}

class SolverTest extends Utils {

  import net.ladstatt.apps.sudoku.BruteForceSolver._

  val solved = """483921657
                 |967345821
                 |251876493
                 |548132976
                 |729564138
                 |136798245
                 |372689514
                 |814253769
                 |695417382""".stripMargin.replaceAll("\n", "")

  @Test def testSum() = {
    assertEquals(405, solved.toArray.map(_.asDigit).sum.toLong)
  }

  def solveReadableSudoku(sudokuWithNewLines: String): Option[SudokuDigitSolution] = {
    solve(sudokuWithNewLines.replaceAll("\n", "").toCharArray)
  }

  @Test
  def testSolving(): Unit = {
    Try {
      val solvedSudokus: Array[Option[SudokuDigitSolution]] =
        for (sudokuAsString <- easySudokus.split("========"))
        yield solveReadableSudoku(sudokuAsString)

      for (fs <- solvedSudokus.flatten) {
        assertEquals(405, fs.map(_.asDigit).sum.toLong)
      }
    } match {
      case Success(_) =>
      case Failure(e) => fail(e.getMessage)
    }

  }

  /**
   * Test shows what happenes if we try to solve an malformed input
   */
  @Test
  def testSolveWrongInput(): Unit = {
    val sudokuInput = """003020601
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
      case None => fail("Could not solve")
    }
  }

  // see http://norvig.com/easy50.txt
  // and also make sure to visit http://norvig.com/sudoku.html
  val easySudokus =
    Source.fromInputStream(getClass.getResourceAsStream("/easysudokus.txt")).getLines().mkString("\n")

}