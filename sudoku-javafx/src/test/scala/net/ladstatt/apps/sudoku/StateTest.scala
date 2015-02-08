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

  def compare(a: SCell, b: SCell): Boolean = {
    false
  }

  def compare(a: SudokuState, b: SudokuState): Boolean = {
      (a.cells.size == b.cells.size) &&
      (a.cells zip b.cells).forall { case x => x._1.equals(x._2)}
    // && (a.digitLibrary == b.digitLibrary)
  }



  def asMat(i: Int): Mat = TemplateLoader.templateLibrary(i)

  val cellData = new Mat
  val cellz = Seq(SCell(1, 112.1212,null), SCell(4, 80.1212,null))
  val s0 = SudokuState(cells = cellz)



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
    for (m <- TemplateLoader.templateLibrary.values) {
      assertEquals(m.dump, m.dump)
    }
  }

  // should test we can compare two non empty mats succesfully
  @Test def testDifferingMats(): Unit = {
    for (m <- TemplateLoader.templateLibrary.values) {
      assertEquals((false, "(1/0/0) != (1/25/50)"), compareMat(new Mat, m))
    }
  }

}
