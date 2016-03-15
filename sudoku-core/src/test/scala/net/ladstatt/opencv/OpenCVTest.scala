package net.ladstatt.opencv

import org.junit.Test
import org.opencv.core.{Point, Rect}

/**
  * Created by lad on 22.02.16.
  */
class OpenCVTest {

  OpenCV.loadNativeLib()

  val rect = new Rect(0, 0, 10, 10)

  @Test def isEmpty(): Unit = {
    assert(!OpenCV.isSomewhatSquare(Seq()))
  }


  @Test def isSomewhatSquare(): Unit = {
    assert(OpenCV.isSomewhatSquare(Seq(new Point(0,0),new Point(10,0), new Point(10,10), new Point(0,10))))
  }
}
