package net.ladstatt.sudoku

import java.io.File

import net.ladstatt.opencv.OpenCV
import org.junit.Assert._
import org.junit.Test

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source


/**
 * Created by lad on 05.05.14.
 */
class SudokuTest  {

  OpenCV.loadNativeLib("../lib/libopencv_java310.so")

  val refCellNumbers: Seq[(Int, Double)] = {
    val lines: Iterator[String] = Source.fromFile(new File("src/test/resources/net/ladstatt/sudoku/sudoku_1_ref.csv")).getLines
    (for (l <- lines) yield {
      val a = l.split(',')
      (a(0).toInt, a(1).toDouble)
    }).toSeq
  }


  // compares individual detection results with a reference file
  @Test def testDetect(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    assertEquals(81.toLong, refCellNumbers.size.toLong)
    val cells: Seq[SCell] = Await.result(SudokuTestContext.sudoku_1.sRectangle.detectedCells, Duration.Inf)
    var i = 0
    for (c <- cells) {
      assertEquals(refCellNumbers(i)._1.toLong, c.value.toLong)
      assertEquals(refCellNumbers(i)._2, c.quality, 0.000001D)
      i = i + 1
    }
  }



}
