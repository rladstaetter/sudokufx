package net.ladstatt.opencv

import org.junit.Assert._
import org.junit.Test
import org.opencv.core.{MatOfPoint, Point, Rect}

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
}
