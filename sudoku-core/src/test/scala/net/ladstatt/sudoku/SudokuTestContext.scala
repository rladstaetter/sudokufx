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

  // see http://norvig.com/easy50.txt
  // and also make sure to visit http://norvig.com/sudoku.html
  val easySudokus =
    Source.fromInputStream(getClass.getResourceAsStream("easysudokus.txt")).getLines().mkString("\n")

  lazy val frameSudoku_1: Mat = Imgcodecs.imread("src/test/resources/net/ladstatt/sudoku/sudoku_1.png")
  lazy val solutionSudoku_1 =
    """617948532
      |524361879
      |389725641
      |742816953
      |156439728
      |938257416
      |275683194
      |461592387
      |893174265""".stripMargin.replaceAll("\n", "").toCharArray
  lazy val sudoku_1 = SCandidate(nr = 0, frame = frameSudoku_1)

  lazy val (sudoku_1Result, _, _) = Await.result(sudoku_1.calc(Map(), Map(), 1, 17, 5000L), Duration.Inf)

  lazy val emptyFrame = new Mat(1280, 768, CvType.CV_8UC3)
  lazy val emptySudoku = SCandidate(nr = 0, frame = emptyFrame)
  lazy val (emptySudokuResult, _, _) = Await.result(emptySudoku.calc(Map(), Map(), 1, 1, 5000L), Duration.Inf)


}
