package net.ladstatt.apps.sudoku

import net.ladstatt.core.SystemEnv
import org.junit.Assert._
import org.junit.Test
import org.opencv.core.Mat
import org.opencv.highgui.Highgui

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


trait OpenCvUnitTest extends SudokuOpenCVUtils {
  override def runtimeNativeLibName =
    if (SystemEnv.runOnMac)
      "/Users/lad/Documents/net.ladstatt/opencv/src/main/lib/mac/libopencv_java246.dylib"
    else if (SystemEnv.isX64) {
      "target/lib/win/x64/opencv_java246.dll"
    } else {
      "target/lib/win/x86/opencv_java246.dll"
    }

  loadNativeLib()
}

/**
 * Created by lad on 05.05.14.
 */
class SudokuTest extends OpenCvUnitTest {


  @Test def testSimple(): Unit = {
    val input: Mat = Highgui.imread("src/test/resources/frame69.png")

    Try {
      val sudokuState = SudokuState(input, 1, 17)
      Await.result(sudokuState.calc, Duration.Inf)
    } match {
      case Success(FrameSuccess(_, _, solutionString, _, _, _)) => {
        assertEquals( """617948532
                        |524361879
                        |389725641
                        |742816953
                        |156439728
                        |938257416
                        |275683194
                        |461592387
                        |893174265""".stripMargin, solutionString)
      }
      case Failure(e) => {
        e.printStackTrace()
        fail(e.getMessage)
      }
      case _ => ???
    }

  }
}
