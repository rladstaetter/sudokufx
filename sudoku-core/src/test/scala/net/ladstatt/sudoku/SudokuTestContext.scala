package net.ladstatt.sudoku

import org.opencv.core.{CvType, Mat}
import org.opencv.imgcodecs.Imgcodecs

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source

/**
  * Provides Test data for Unit Tests
  *
  * Created by lad on 06.01.16.
  */
object SudokuTestContext {

  val defaultDigitLibrary: DigitLibrary = Map().withDefaultValue((Double.MaxValue, None))
  val defaultHitCounts: HitCounters = Map().withDefaultValue(Map[Int, Int]().withDefaultValue(0))

  // see http://norvig.com/easy50.txt
  // and also make sure to visit http://norvig.com/sudoku.html
  lazy val easySudokus =
    Source.fromInputStream(getClass.getResourceAsStream("easysudokus.txt")).getLines().mkString("\n")

  val solutionSudoku_1 =
    """617948532
      |524361879
      |389725641
      |742816953
      |156439728
      |938257416
      |275683194
      |461592387
      |893174265""".stripMargin.replaceAll("\n", "").toCharArray

  lazy val frameSudoku_1: Mat = Imgcodecs.imread("src/test/resources/net/ladstatt/sudoku/sudoku_1.png")
  lazy val emptyFrame: Mat = new Mat(1280, 768, CvType.CV_8UC3)

  lazy val (sudoku_1, (sudoku_1Result,  _)) = calculate(frameSudoku_1)
  lazy val (emptySudoku, (emptySudokuResult, _)) = calculate(emptyFrame)

  def calculate(frame: Mat): (SCandidate, (SudokuResult, SudokuState)) = {
    val c = SCandidate(0, FramePipeline(frame))
    (c, Await.result(c.calc(Parameters.DefaultState, 1, 17, 5000L), Duration.Inf))
  }

}
