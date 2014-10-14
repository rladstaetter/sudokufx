package net.ladstatt.apps.sudoku

import net.ladstatt.core.Utils
import org.junit.Assert._
import org.junit.{Ignore, Test}
import org.opencv.core.{CvType, Mat}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by lad on 27.04.14.
 */
class HistoryTest extends OpenCvUnitTest with Utils {

  import Parameters._

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

    Await.result(
      h.computeSolution(mockMat, (for (p <- positions) yield SCell(getAt(p).toInt, 0.1, new Mat)).toArray)
      , Duration.Inf)
    assertTrue(h.detectedNumbers.size > h.minHits)

    val result = h.mkValueMatrix

    for (p <- positions) {
      assertTrue(getAt(p) == result.flatten.apply(p))
    }
  }


  @Test def detectInvalidSector(): Unit = {
    val h = SudokuState(0, mockMat, 1, 1)
    val invalidList =
      Array(SCell(1, 0.1, new Mat), SCell(1, 0.1, new Mat))
    val r = Await.result(h.computeSolution(new Mat, invalidList), Duration.Inf)
    assertTrue(0 == h.hitCounts(0)(0))
  }

  @Test def libraryTest(): Unit = {
    val state = SudokuState(0, mockMat, 1, 1)
    val validArray =
      Array(SCell(1, 0.5, new Mat), SCell(2, 0.8, new Mat)
      )
    state.updateDigitLibrary(validArray)

    state.updateDigitLibrary(Array(SCell(1, 0.5, new Mat), SCell(2, 0.7, new Mat)))
      assertEquals(0.5, state.digitQuality(1), 0.0)
      assertEquals(0.7, state.digitQuality(2), 0.0)

  }

  @Test def detectEmptyCells() = {
    val h = SudokuState(0, null, 1, 1)
    val partialSolution: Cells = Array( SCell(0, 0, new Mat))
    Await.result(h.computeSolution(mockMat, partialSolution), Duration.Inf)
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
