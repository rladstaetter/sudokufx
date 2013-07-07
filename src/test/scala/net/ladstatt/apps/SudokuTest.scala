package net.ladstatt.apps

import org.junit.{Ignore, Test}
import java.io.File
import org.opencv.core.CvType
import org.junit.Assert._

/**
 * Created by lad on 03.06.13.
 */
class SudokuTest extends Sudokuaner {

  val loadLibs = loadNativeLibs()

  def compareKleine = fullTextCompare(KleineZeitung) _


  @Test def kleineTest1() = compareKleine("sudoku1.png")

  @Test def kleineTest2() = compareKleine("sudoku2.png")

  @Test def kleineTest3() = compareKleine("sudoku3.png")

  @Test def kleineTest4() = compareKleine("sudoku4.png")

  @Test def kleineTest5() = compareKleine("sudoku5.png")


  @Test def sumKleineDifferencesTest() = {
    val std = KleineZeitung
    assertEquals(0, (1 to std.data.size).map("sudoku%s.png".format(_)).map(levi(std, _)).sum)
  }


  def levi(sudokuTestData: SudokuTestData, fileName: String): Int = {
    val expected = sudokuTestData.data(fileName)
    val (warped, corners,cells) = mkSudoku(input = readImage(new File(sudokuTestData.resPath, fileName), CvType.CV_8UC1),
      // detectNumberMethod = withFeatureExtraction(mkComparisonLibrary(sudokuTestData.trainingPath)))
      detectNumberMethod = withTemplateMatching(mkTemplateLibrary(sudokuTestData.trainingPath)))
    levensthein(toString(cells), expected)
  }

  def fullTextCompare(sudokuTestData: SudokuTestData)(fileName: String) = {
    val expected = sudokuTestData.data(fileName)
    val (warped, corners,cells) = mkSudoku(input = readImage(new File(sudokuTestData.resPath, fileName), CvType.CV_8UC1),
      //  detectNumberMethod = withFeatureExtraction(mkComparisonLibrary(sudokuTestData.trainingPath)))
      detectNumberMethod = withTemplateMatching(mkTemplateLibrary(sudokuTestData.trainingPath)))

    val lev = levensthein(toString(cells), expected)
    assertEquals("Distance: %s".format(lev), expected, toString(cells))
  }

}
