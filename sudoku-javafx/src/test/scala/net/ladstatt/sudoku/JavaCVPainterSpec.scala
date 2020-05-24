package net.ladstatt.sudoku

import org.bytedeco.opencv.opencv_core.Mat
import org.scalatest.wordspec.AnyWordSpecLike

class JavaCVPainterSpec extends AnyWordSpecLike {

  "JavaCVPainter" should {
    /** code throws a Runtime exception since given Mat doesn't conform to expected structure, but no nullpointer */
    "throw no Nullpointer" in {
      intercept[RuntimeException] {
        JavaCVPainter.toImage(new Mat)
      }
    }
  }
}
