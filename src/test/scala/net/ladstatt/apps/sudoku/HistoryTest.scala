package net.ladstatt.apps.sudoku

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
class HistoryTest extends OpenCvUnitTest {

  import Parameters._


  val sudoku =
    """245981376
      |169273584
      |837564219
      |976125438
      |513498627
      |482736951
      |391657842
      |728349165
      |654812793""".stripMargin.replaceAll("\n", "")

  def getAt(pos: Pos): Int = sudoku(pos._2 + pos._1 * 9).toInt - 48

  @Test def aTestWalkthrough(): Unit = {
    val cap = 1
    val h = SudokuState(new Mat,cap, 20)

    Await.result(
      h.computeSolution(new Mat, (for (p <- positions) yield p -> SCell(getAt(p), 0.1, new Mat)).toMap)
      , Duration.Inf)
    assertTrue(h.detectedNumbers.size > h.minHits)

    val result = h.mkValueMatrix

    for (p <- positions) {
      assertTrue(getAt(p) == result(p))
    }
  }

  val mockMat = {
    new Mat(1280, 768, CvType.CV_8UC3)
  }

  @Test def detectInvalidSector(): Unit = {
    val h = SudokuState(null,1, 1)
    val invalidList =
      Map((0, 0) -> SCell(1, 0.1, new Mat),
        (0, 1) -> SCell(1, 0.1, new Mat))
    val r = Await.result(h.computeSolution(new Mat, invalidList), Duration.Inf)
    assertTrue(0 == h.posFrequencies(0)(0))
  }

  @Test def libraryTest(): Unit = {
    val h = SudokuState(null,1, 1)
    val validlist =
      Map((0, 0) -> SCell(1, 0.5, new Mat),
        (4, 1) -> SCell(2, 0.8, new Mat))
    h.updateDigitLibrary(validlist)

    h.updateDigitLibrary(Map((4, 1) -> SCell(2, 0.7, new Mat)))
    assertEquals(0.5, h.digitLibrary(1)._2, 0.0)
    assertEquals(0.7, h.digitLibrary(2)._2, 0.0)

  }

  @Test def detectEmptyCells() = {
    val h = SudokuState(null,1, 1)
    val partialSolution: Cells = Map((0, 0) -> SCell(0, 0, new Mat))
    Await.result(h.computeSolution(mockMat, partialSolution), Duration.Inf)
    assertTrue(1 == h.posFrequencies(0)(0))
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
