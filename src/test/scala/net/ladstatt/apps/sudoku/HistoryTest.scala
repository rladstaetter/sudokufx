package net.ladstatt.apps.sudoku

import net.ladstatt.core.Utils
import org.junit.Assert._
import org.junit.{Ignore, Test}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}

/**
 * Created by lad on 27.04.14.
 */
class HistoryTest extends OpenCvUnitTest with Utils {

  val sudokuAsString =
    """245981376
      |169273584
      |837564219
      |976125438
      |513498627
      |482736951
      |391657842
      |728349165
      |654812793""".stripMargin.replaceAll("\n", "")

  val expectedDigitSolution: SudokuDigitSolution = sudokuAsString.toCharArray

  val sudokuflat: Array[Char] = expectedDigitSolution

  val emptySudoku = SCandidate(0, emptyFrame, 1, 1)

  val emptySudokuResult = time(Await.result(emptySudoku.calc(), Duration.Inf), t => println(s"emptyFrame: $t ms"))

  val sudoku69 = SCandidate(0, frame69, 1, 20)

  @Ignore
  @Test def aTestWalkthrough(): Unit = {
    Await.result(sudoku69.calc(), Duration.Inf) match {
      case s: SSuccess => {
        //assertTrue(h.detectedNumbers.size > h.minHits)
        assertEquals(sudokuAsString, s.solutionAsString)
      }
      case SFailure(_) => fail()
    }
  }


  @Test def detectInvalidSector(): Unit = {
    val r = Await.result(emptySudoku.computeSolution(), Duration.Inf)
    assertTrue(0 == emptySudoku.hitCounts(0)(0))
  }

  @Test def detectEmptyCells() = {
    assertEquals( """Hitcounts :
                    |-----------
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |0,0,0,0,0,0,0,0,0,0
                    |
                    |""".stripMargin, emptySudokuResult match {
      case SFailure(candidate) => candidate.statsAsString()
      case SSuccess(candidate, _, _) => candidate.statsAsString()
    })
  }

  @Test
  def invalidSCell() = {
    Try {
      SCell(-10, 0, null)
    } match {
      case Failure(e: AssertionError) =>
      case _ => fail("should throw an AssertionError")
    }
  }

  @Test
  def invalidSCell2() = {
    Try {
      SCell(0, -1, null)
    } match {
      case Failure(e: AssertionError) =>
      case _ => fail("should throw an AssertionError")
    }
  }

}
