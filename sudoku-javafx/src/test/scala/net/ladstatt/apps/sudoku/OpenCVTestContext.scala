package net.ladstatt.apps.sudoku

import net.ladstatt.core.{CanLog, Utils}
import org.opencv.core.{CvType, Mat}
import org.opencv.imgcodecs.Imgcodecs

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Provides Test data for Unit Tests
  *
  * Created by lad on 26.10.14.
  */
object OpenCVTestContext extends CanLog {


  lazy val frame69: Mat = Imgcodecs.imread("src/test/resources/frame69.png")
  lazy val solution69 =
    """617948532
      |524361879
      |389725641
      |742816953
      |156439728
      |938257416
      |275683194
      |461592387
      |893174265""".stripMargin.replaceAll("\n", "").toCharArray
  lazy val sudoku69 = SCandidate(nr = 0, frame = frame69)

  lazy val (sudoku69Result, _, _) = Await.result(sudoku69.calc(Map(), Map(), 1, 17, 5000L), Duration.Inf)

  lazy val emptyFrame = new Mat(1280, 768, CvType.CV_8UC3)
  lazy val emptySudoku = SCandidate(nr = 0, frame = emptyFrame)
  lazy val (emptySudokuResult, _, _) = Await.result(emptySudoku.calc(Map(), Map(), 1, 1, 5000L), Duration.Inf)


}
