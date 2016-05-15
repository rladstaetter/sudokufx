package net.ladstatt.sudoku

import net.ladstatt.opencv.OpenCV
import org.junit.Assert._
import org.junit.Test
import org.opencv.core.{Mat, Rect}

/**
  * Created by lad on 12.01.15.
  */
class StateTest {

  OpenCV.loadNativeLib("../lib/libopencv_java310.so")

  def compare(a: SCell, b: SCell): Boolean = {
    false
  }

  def asMat(i: Int): Mat = TemplateLibrary.asSeq(i)

  val cellData = SudokuTestContext.frameSudoku_1
  val cellz = Seq(SCell(cellData, new Rect), SCell(cellData, new Rect))


  // defines a poor man's comparison between matrixes
  def compareMat(a: Mat, b: Mat): (Boolean, String) = {
    val (aCols, aRows) = (a.cols, a.rows)
    val (bCols, bRows) = (b.cols, b.rows)
    val (aChan, bChan) = (a.channels(), b.channels())

    if ((aChan == bChan) && (aCols == bCols) && (aRows == bRows)) {
      if (a.dump == b.dump) {
        (true, "OK")
      } else (false, "matrices differ")
    } else (false, s"($aChan/$aCols/$aRows) != ($bChan/$bCols/$bRows)")
  }

  // should show that we can compare two empty mats successfully
  @Test def testCompareMatForEmptyMats(): Unit = {
    //assertEquals(112.1212, s0.mergeN(s0).digitLibrary(1)._1, 0.00001)
    //assertEquals(Some(cellData), s0.mergeN(s0).digitLibrary(1)._2)
    assertEquals((true, "OK"), compareMat(new Mat, new Mat))
  }

  // should test we can compare two non empty mats succesfully
  @Test def testTwoNonEmptyMats(): Unit = {
    for (m <- TemplateLibrary.asSeq) {
      assertEquals(m.dump, m.dump)
    }
  }

  // should test we can compare two non empty mats succesfully
  @Test def testDifferingMats(): Unit = {
    for (m <- TemplateLibrary.asSeq) {
      assertEquals((false, "(1/0/0) != (1/25/50)"), compareMat(new Mat, m))
    }
  }

}
