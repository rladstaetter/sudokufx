package net.ladstatt.sudoku

import java.io.{File, FilenameFilter}

import net.ladstatt.opencv.OpenCV
import org.junit.Assert._
import org.junit.Test
import org.opencv.core.{Mat, Point, Rect}
import org.opencv.imgcodecs.Imgcodecs

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source

/**
  * Created by lad on 05.05.14.
  */
class SudokuTest {

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
    assertEquals(81.toLong, refCellNumbers.size.toLong)
    val cells: Seq[SCell] = SudokuTestContext.sudoku_1.sRectangle.cells
    var i = 0
    for (c <- cells) {
      assertEquals(refCellNumbers(i)._1.toLong, c.value.toLong)
      assertEquals(refCellNumbers(i)._2, c.quality, 0.000001D)
      i = i + 1
    }
  }

  /**
    * tests the matching algorithm against a library of individual number pictures
    */
  @Test def testTemplateMatching(): Unit = {
    val libraryPath = new File("/Users/lad/Documents/sudokufx/sudoku-core/src/test/resources/net/ladstatt/sudoku/library")
    for (i <- 0 to 9) {
      val dir = new File(libraryPath, i.toString)
      val someFiles = Option(dir.listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = name.endsWith(".png")
      }))
      someFiles foreach {
        case files =>
          val results: mutable.ArraySeq[(File, (Int, Int, Double))] =
            Await.result(
              Future.sequence(
                for {f <- files} yield {
                  val image: Mat = Imgcodecs.imread(f.getAbsolutePath)
                  val rect = new Rect(new Point(0, 0), image.size)
                  SCell(image, rect).computValueAndQuality.map {
                    case ((n: Int, q: Double)) => (f, (i, n, q))
                  }
                }), Duration.Inf)

          val mismatches = results.filter {
            case (_, (e,a,_)) => e != a
          }

          if (mismatches.nonEmpty) {
            mismatches.foreach {
              case (f, (expected, actual, q)) if expected != actual => println(s"Image ${f.getAbsolutePath} should be $expected, but was $actual.")
              case _ =>
            }
            //fail("Mismatch detected.")
          }
      }
    }
  }


}
