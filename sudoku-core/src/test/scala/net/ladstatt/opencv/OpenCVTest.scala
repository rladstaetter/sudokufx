package net.ladstatt.opencv

import java.io.File

import net.ladstatt.sudoku.{Parameters, SCell}
import org.junit.Assert._
import org.junit.Test
import org.opencv.core.{Mat, MatOfPoint, Point, Rect}
import org.opencv.imgcodecs.Imgcodecs

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Created by lad on 22.02.16.
  */
class OpenCVTest {

  OpenCV.loadNativeLib()

  val rect = new Rect(0, 0, 10, 10)

  @Test def isEmpty(): Unit = {
    assert(!OpenCV.isSomewhatSquare(Seq()))
  }

  /**
    * what happenes if we detect no contours
    */
  @Test def extractMaxCurveFromEmptyInput(): Unit = {
    val res: Option[(Double, MatOfPoint)] = OpenCV.extractCurveWithMaxArea(Seq())
    assertEquals(None, res)
  }

  /**
    * what happenes if we detect a list of curves with only one element.
    */
  @Test def extractMaxCurveWithOneElement(): Unit = {
    val c: MatOfPoint = mkContour(0, 0)
    val seq: Seq[MatOfPoint] = Seq(c)
    val res: Option[(Double, MatOfPoint)] = OpenCV.extractCurveWithMaxArea(seq)
    assert(res.isDefined)
    res foreach {
      case (area, _) => assert(area == 0)
    }
  }

  @Test def extractBiggerContour(): Unit = {
    val c1: MatOfPoint = mkContour(0, 0, 0, 10, 10, 10, 10, 0)
    val c2: MatOfPoint = mkContour(0, 0, 0, 20, 20, 20, 20, 0)
    val seq: Seq[MatOfPoint] = Seq(c1, c2)
    val res: Option[(Double, MatOfPoint)] = OpenCV.extractCurveWithMaxArea(seq)
    assert(res.isDefined)
    res foreach {
      case (area, _) => assertEquals(400, area, 0.0)
    }
  }


  def mkContour(pts: Double*): MatOfPoint = {
    assert(pts.size % 2 == 0)
    val points: Iterator[Point] = for (Seq(x, y) <- pts.sliding(2)) yield new Point(x, y)
    new MatOfPoint(points.toSeq: _*)
  }

  @Test def isSomewhatSquare(): Unit = {
    assert(OpenCV.isSomewhatSquare(Seq(new Point(0, 0), new Point(10, 0), new Point(10, 10), new Point(0, 10))))
  }

  /**
    * expected values for the example sudoku 'frau von heute'
    */
  val expectedFrauVonHeute: Map[Int, Int] = Map(
    0 -> 6, 2 -> 8, 5 -> 1, 7 -> 2,
    11 -> 9, 12 -> 3, 14 -> 2, 15 -> 5, 16 -> 8,
    21 -> 8, 22 -> 9, 24 -> 3,
    30 -> 2, 34 -> 9,
    36 -> 3, 40 -> 8, 44 -> 7,
    46 -> 4, 50 -> 6,
    56 -> 3, 58 -> 2, 59 -> 5,
    64 -> 9, 65 -> 4, 66 -> 1, 68 -> 3, 69 -> 2,
    73 -> 7, 75 -> 6, 78 -> 9, 80 -> 3
  )

  /**
    * range of sudoku frames to look at
    */
  val examplesudokus = 0 to 1

  /**
    * denotes exceptions which are not recognized properly, the key is the index of the example
    * sudoku, the values are again indexes which are not recognized properly.
    *
    * ideally this exception map should be empty.
    *
    * At the moment, the algorithm has major problems recognizing '1', '2', '3',
    **/
  val exceptions: Map[Int, Set[Int]] = Map(
    0 -> Set(5),
    1 -> Set(5, 56, 24, 14, 46, 21, 34, 22, 59, 12, 66, 50, 40, 58, 36, 30),
    2 -> Set(66),
    3 -> Set(5, 66),
    4 -> Set(5, 66),
    5 -> Set(66),
    7 -> Set(66),
    8 -> Set(66),
    9 -> Set(5, 66),
    10 -> Set(5, 66),
    11 -> Set(66),
    12 -> Set(66),
    13 -> Set(66),
    14 -> Set(66),
    15 -> Set(66),
    16 -> Set(66),
    17 -> Set(66),
    18 -> Set(66),
    20 -> Set(66),
    21 -> Set(66),
    24 -> Set(66),
    25 -> Set(66),
    27 -> Set(5, 66),
    30 -> Set(56, 66),
    31 -> Set(5, 56, 46, 66, 58),
    32 -> Set(56, 46, 66, 58, 36),
    33 -> Set(56, 46, 66, 58, 36),
    34 -> Set(56, 46, 66, 58, 36),
    35 -> Set(56, 66),
    36 -> Set(5, 56, 66),
    37 -> Set(5, 56, 46, 66, 58, 36),
    38 -> Set(56, 46, 66, 58, 36),
    39 -> Set(56, 46, 66, 58, 36),
    40 -> Set(56, 66),
    41 -> Set(56, 66),
    42 -> Set(56, 46, 66, 58, 36),
    43 -> Set(56, 46, 66, 58, 36),
    44 -> Set(56, 46, 66, 58, 36),
    45 -> Set(56, 46, 66, 58, 36),
    46 -> Set(56, 66, 58),
    47 -> Set(56, 66, 58),
    48 -> Set(56, 66, 58),
    49 -> Set(0, 66, 80),
    50 -> Set(66),
    51 -> Set(56, 66),
    52 -> Set(56, 59, 66, 75, 58, 36, 68),
    53 -> Set(56, 73, 66),
    54 -> Set(56, 66),
    55 -> Set(56, 66),
    56 -> Set(66),
    57 -> Set(56, 66),
    58 -> Set(56, 66),
    60 -> Set(56, 66),
    61 -> Set(56, 66),
    62 -> Set(66),
    64 -> Set(66),
    66 -> Set(66),
    67 -> Set(66),
    68 -> Set(66),
    69 -> Set(66),
    70 -> Set(66),
    71 -> Set(5, 66),
    72 -> Set(5, 66),
    73 -> Set(66),
    74 -> Set(56, 66),
    75 -> Set(66),
    76 -> Set(66),
    77 -> Set(5, 66),
    78 -> Set(66),
    79 -> Set(66),
    80 -> Set(66),
    82 -> Set(66),
    83 -> Set(5, 66),
    84 -> Set(66),
    85 -> Set(66),
    86 -> Set(66),
    87 -> Set(66),
    88 -> Set(66, 36),
    89 -> Set(46, 36),
    90 -> Set(46, 66, 36),
    91 -> Set(46, 66, 36),
    92 -> Set(66, 36),
    93 -> Set(46, 66, 36),
    94 -> Set(46, 36),
    95 -> Set(46, 66, 36),
    96 -> Set(66, 36),
    97 -> Set(5, 46, 66, 36),
    98 -> Set(46, 66, 36),
    99 -> Set(46, 66, 36),
    100 -> Set(46, 66, 36),
    101 -> Set(46, 66, 36),
    102 -> Set(36),
    103 -> Set(46, 36),
    104 -> Set(5, 46, 36)
  ).withDefaultValue(Set())


  /**
    * Checks that a series of preprocessed inputs of the same sudoku is recognized properly.
    */
  @Test def normalizedTest(): Unit = {
    val nr = 0
    for (nr <- examplesudokus) {
      val image: File = new File(s"src/test/resources/net/ladstatt/sudoku/normalized/$nr/normalized.png")
      assert(image.exists(),image.getAbsolutePath)
      val m = Imgcodecs.imread(image.getAbsolutePath)
      check(nr, m, expectedFrauVonHeute -- exceptions(nr))
    }
  }

  def check(sudokuNr: Int, m: Mat, expected: Map[Int, Int]): Unit = {
    val cWidth: Int = (m.size.width / Parameters.ssize).toInt
    val cHeight: Int = (m.size.height / Parameters.ssize).toInt
    for ((idx, v) <- expected) {
      val SCell(value, _, _) = Await.result(OpenCV.detectCell(m, OpenCV.mkRect(idx, cWidth, cHeight)), Duration.Inf)
      assertEquals(s"sudoku_$sudokuNr, idx: $idx >> Expected $v in row ${Parameters.row(idx)} and col ${Parameters.col(idx)}, but got $value.", v, value)
    }

  }

}
