package net.ladstatt.sudoku

import java.io.File

import net.ladstatt.opencv.OpenCV
import org.scalatest.WordSpecLike

import scala.io.Source

/**
 * Created by lad on 05.05.14.
 */
class SudokuSpec extends WordSpecLike {

  OpenCV.loadNativeLib()

  val refCellNumbers: Seq[(Int, Double)] = {
    val lines: Iterator[String] = Source.fromFile(new File("src/test/resources/net/ladstatt/sudoku/sudoku_1_ref.csv")).getLines
    (for (l <- lines) yield {
      val a = l.split(',')
      (a(0).toInt, a(1).toDouble)
    }).toSeq
  }


  // compares individual detection results with a reference file
  "testDetect" ignore {
    assert(81.toLong == refCellNumbers.size.toLong)
    val cells: Seq[SCell] = SudokuTestContext.sudoku_1.sRectangle.cells
    var i = 0
    for (c <- cells) {
      assert(refCellNumbers(i)._1.toLong == c.value.toLong)
      assert(Math.abs(refCellNumbers(i)._2 - c.quality) < 0.000001D)
      i = i + 1
    }
  }

}
