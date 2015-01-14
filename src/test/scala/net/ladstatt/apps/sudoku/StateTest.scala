package net.ladstatt.apps.sudoku

import net.ladstatt.opencv.OpenCV
import org.junit.Assert._
import org.junit.Test
import org.opencv.core.Mat

/**
 * Created by lad on 12.01.15.
 */
class StateTest {

  OpenCV.loadNativeLib()

  def compare(a: SudokuState, b: SudokuState): Boolean = {
    (a.hCounts.deep == b.hCounts.deep) &&
      (a.digitQuality.deep == b.digitQuality.deep) &&
      (a.digitData.deep == b.digitData.deep) &&
      (a.cap == b.cap) &&
      (a.minHits == b.minHits) &&
      (a.cells == b.cells)
  /*
      */
   // && (a.digitLibrary == b.digitLibrary)
  }

  @Test def stateMergeTest(): Unit = {
    assertTrue(compare(SudokuState(), SudokuState().mergeN(SudokuState())))
  }

  val cellData = new Mat
  val cellz = Seq(SCell(1, 112.1212, cellData))
  val s0 = SudokuState(cells = cellz)
/*
  @Test def stateMergeTest14(): Unit = assertEquals(SudokuState(), SudokuState().mergeN(SudokuState()))
  @Test def stateMergeTest1(): Unit = assertEquals(SudokuState(), SudokuState())
  @Test def stateMergeTest1_0(): Unit = assertEquals(s0, SudokuState().mergeN(s0))
  @Test def stateMergeTest1_1(): Unit = assertEquals(s0, s0.mergeN(SudokuState()))
  @Test def stateMergeTest1_2(): Unit = assertEquals(s0, s0.mergeN(s0))
  */
  @Test def stateMergeTest1(): Unit = assertTrue(compare(s0, SudokuState().mergeN(s0)))
  @Test def stateMergeTest2(): Unit = assertTrue(compare(s0, s0.mergeN(SudokuState())))
  @Test def stateMergeTest3(): Unit = assertTrue(compare(s0, s0.mergeN(s0)))


  @Test def testSudokuStateLibrary(): Unit = {
    assertEquals(112.1212, s0.mergeN(s0).digitLibrary(1)._1, 0.00001)
    assertEquals(Some(cellData), s0.mergeN(s0).digitLibrary(1)._2)
  }

}
