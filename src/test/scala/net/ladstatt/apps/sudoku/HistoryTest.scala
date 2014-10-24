package net.ladstatt.apps.sudoku

import net.ladstatt.core.Utils
import org.junit.Assert._
import org.junit.Test
import org.opencv.core.Mat

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}

/**
 * Created by lad on 27.04.14.
 */
class HistoryTest extends OpenCvUnitTest with Utils {

  import net.ladstatt.apps.sudoku.Parameters._

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

  val sudoku = mkDigitSolution(sudokuAsString)

  // TODO remove
  def getAt(pos: Pos): Char = sudoku.flatten.apply(pos)

  @Test def aTestWalkthrough(): Unit = {
    val cap = 1
    val h = SudokuState(0, mockMat, cap, 20)
    val sCells = (for (p <- positions) yield SCell(getAt(p).asDigit, 0.1, new Mat))
    h.countHits(sCells)
    val someSolution = Await.result(h.computeSolution(), Duration.Inf)
    //assertTrue(h.detectedNumbers.size > h.minHits)
    assertTrue(someSolution.isDefined)
    for (s <- someSolution) {
      for (p <- positions) {
        assertEquals(mkStringSolution(sudoku), mkStringSolution(s))
      }
    }
  }


  @Test def detectInvalidSector(): Unit = {
    val h = SudokuState(0, mockMat, 1, 1)
    val invalidList = Array(SCell(1, 0.1, new Mat), SCell(1, 0.1, new Mat))
    val r = Await.result(h.computeSolution(), Duration.Inf)
    assertTrue(0 == h.hitCounts(0)(0))
  }

  @Test def libraryTest(): Unit = {
    val state = SudokuState(0, mockMat, 1, 1)
    val validArray =
      Array(SCell(1, 0.5, new Mat), SCell(2, 0.8, new Mat)
      )
    state.updateLibrary(validArray)

    state.updateLibrary(Array(SCell(1, 0.5, new Mat), SCell(2, 0.7, new Mat)))
    assertEquals(0.5, state.digitQuality(1), 0.0)
    assertEquals(0.7, state.digitQuality(2), 0.0)

  }

  @Test def detectEmptyCells() = {
    val h = SudokuState(0, mockMat, 1, 1)
    val partialSolution: Cells = Array(SCell(0, 0, new Mat))
    h.countHits(partialSolution)
    assertTrue(1 == h.hitCounts(0)(0))
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
