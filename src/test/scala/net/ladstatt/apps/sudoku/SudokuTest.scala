package net.ladstatt.apps.sudoku

import net.ladstatt.core.Utils
import net.ladstatt.opencv.OpenCV
import org.junit.Assert._
import org.junit.Test
import org.opencv.core.{CvType, Mat}
import org.opencv.highgui.Highgui

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}




/**
 * Created by lad on 05.05.14.
 */
class SudokuTest extends OpenCvUnitTest with Utils {


  @Test def testSimple(): Unit = {

    Try {
      val sudoku69 = Sudoku(0, frame69, 1, 17)
      Await.result(sudoku69.calc, Duration.Inf)
    } match {
      case Success(s) if s.someResult.isDefined => {
        assertEquals( """617948532
                        |524361879
                        |389725641
                        |742816953
                        |156439728
                        |938257416
                        |275683194
                        |461592387
                        |893174265""".stripMargin, mkStringSolution(s.someResult.get.digitSolution.get))
      }
      case Failure(e) => {
        e.printStackTrace()
        fail(e.getMessage)
      }
      case _ => ???
    }

  }
}
