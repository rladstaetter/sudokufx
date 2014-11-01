package net.ladstatt.apps.sudoku

import java.io.File

import net.ladstatt.core.Utils
import org.junit.Assert._
import org.junit.Test

import scala.io.Source
import scala.util.{Failure, Success, Try}

class SolverTest extends Utils {

  import net.ladstatt.apps.sudoku.SudokuAlgos.BruteForceSolver._

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

  @Test
  def testSolving(): Unit = {
    Try {
      val solvedSudokus: Array[Option[SudokuDigitSolution]] =
        for (sudokuAsString <- easySudokus.split("========"))
        yield solve(sudokuAsString.replaceAll("\n", "").toCharArray)

      for (fs <- solvedSudokus.flatten) {
        assertEquals(405, fs.map(_.asDigit).sum.toLong)
      }
    } match {
      case Success(_) =>
      case Failure(e) => fail(e.getMessage)
    }

  }

  // see http://norvig.com/easy50.txt
  // and also make sure to visit http://norvig.com/sudoku.html
  val easySudokus =
    Source.fromInputStream(getClass.getResourceAsStream("/easysudokus.txt")).getLines().mkString("\n")

}