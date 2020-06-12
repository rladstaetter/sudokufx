package net.ladstatt.sudoku

import java.nio.file.Paths

import net.ladstatt.core.CanLog
import org.scalatest.wordspec.AnyWordSpecLike


object Sudokus {

  private val pathPrefix = "src/test/resources/net/ladstatt/sudoku/testdata/"

  val frame1 = Paths.get(pathPrefix + "frame1.png")
  val frame2 = Paths.get(pathPrefix + "frame2.png")

  /** the real deal. Test sudoku looks like this. Ideally, the algorithm should detect exactly those numbers */
  val sudokuUnsolvedExact: SudokuConfiguration =
    SudokuConfiguration(
      """|608001020
         |009302580
         |000890300
         |000200090
         |300080007
         |040006000
         |003025000
         |094103200
         |070600903""".stripMargin)

  /* sudoku with 100% hit rate */
  val sudoku1Empty =
    Sudoku("sudoku1"
      , frame1
      , sudokuUnsolvedExact
      , Seq(862f, 283f
        , 1240f, 339f
        , 1172f, 711f
        , 804f, 640f))
}

class SudokuSpec extends AnyWordSpecLike with CanLog {

  import Sudokus._

  /** what the image processing thinks it sees for frame1 */
  val sudoku1CurrentBest =
    sudoku1Empty.copy(sudokuConfiguration =
      SudokuConfiguration(
        """|608000020
           |009362580
           |000890300
           |000200090
           |000080000
           |040006000
           |003025000
           |094203200
           |070600900""".stripMargin))

  /** what the image processing thinks it sees for frame2 */
  val sudoku2CurrentBest =
    Sudoku("sudoku2"
      , frame2
      , SudokuConfiguration(
        """|608001020
           |009302580
           |000890300
           |000200090
           |300080007
           |040006000
           |003025000
           |094103200
           |070600903""".stripMargin)
      , corners = Seq(
        333.0f, 192.0f
        , 719.0f, 170.0f
        , 738.0f, 571.0f
        , 349.0f, 588.0f
      ))

  /** a map with paths to original images and coordinates of sudoku area to detect */
  val validSudokus: Seq[Sudoku] = Seq(sudoku1CurrentBest, sudoku2CurrentBest)

  "SudokuCanvas" should {
    /**
     * given an input frame, detectSudoku canvas
     */
    "be detected correctly" in {
      for (s <- validSudokus) {
        SudokuUtils.detectSudokuCanvas(s) match {
          case Some(canvas) =>
            JavaCV.writeMat(Paths.get("target/normalized.png"), canvas.normalized)
            assert(canvas.corners == s.corners)
            assert(canvas.cells.size == 81)
            assert(canvas.sudokuConfiguration == s.sudokuConfiguration)
          case None => fail("Could not detect sudoku canvas")
        }
      }
    }

    "return a Sudoku Candidate" in {
      for (s <- Seq(sudoku2CurrentBest)) {
        SResult(0, SudokuState(), s.contourParams, s.pipeline) match {
          case fp: FramePipeline =>
            fail("Could not detect sudoku")
          case sc: SCandidate =>
            sc.calc match {
              case SSuccess(_, _, None) =>
                fail("could not find solution")
              case SSuccess(inputFrame, sudokuFrame, Some(solutionFrame)) =>
                logInfo("found solution")
                assert(solutionFrame.solvedState.detections > 0)
                JavaCV.writeMat(Paths.get(s"target/${s.id}-solution.png"), solutionFrame.solutionMat)
              case SFailure(inputFrame, msg) =>
                fail("failed to calculate solution")
            }
        }
      }
    }
  }

  /*
    val refCellNumbers: Seq[(Int, Double)] = {
      val lines: Iterator[String] = Source.fromFile(new File("src/test/resources/net/ladstatt/sudoku/sudoku_1_ref.csv")).getLines
      (for (l <- lines) yield {
        val a = l.split(',')
        (a(0).toInt, a(1).toDouble)
      }).toSeq
    }


    // compares individual detection results with a reference file
    "testDetect" ignore {
      assert(81.toLong == refCellNumbers.size.toLong)
      val cells: Seq[SCell] = SudokuTestContext.sudoku_1.sRectangle.cells
      var i = 0
      for (c <- cells) {
        assert(refCellNumbers(i)._1.toLong == c.value.toLong)
        assert(Math.abs(refCellNumbers(i)._2 - c.quality) < 0.000001D)
        i = i + 1
      }
    }

  */
}
