package net.ladstatt.sudoku

import java.nio.file.{Files, Path, Paths}

import net.ladstatt.core.CanLog
import org.bytedeco.javacpp.FloatPointer
import org.bytedeco.opencv.global.{opencv_core, opencv_imgproc}
import org.bytedeco.opencv.opencv_core.{Mat, Rect, Size}
import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpecLike

import scala.jdk.CollectionConverters._


class SudokuSpec extends AnyWordSpecLike with CanLog {

  import Sudokus._


  "SudokuHistory" should {
    "equals works" in {
      assert(SudokuState() == SudokuState())
      assert(SudokuState().equals(SudokuState()))
    }
    "equals test2" in assert(Sudokus.sudokuSolved == Sudokus.sudokuSolved)
    "equals test3" in assert(Sudokus.s1ReadyToSolve == Sudokus.s1ReadyToSolve)
    "equals test4" in assert(Sudokus.s1ReadyToSolve.solved == Sudokus.s1ReadyToSolve.solved)
    "reference sudoku has 81 hits" in assert(sudokuSolved.nrHits == 81)
    "a solved sudoku returns itself again" in assert(sudokuSolved.solved == sudokuSolved)
    "an unsolved sudoku turns into a solved one" in {
      val s1Solved: SudokuState = s1ReadyToSolve.solved
      val solvedSudoku = Sudokus.sudokuSolved
      assert(s1Solved.asSudokuString == solvedSudoku.assumeReadyToSolve.asSudokuString)
      assert(s1Solved.equals(solvedSudoku))
    }
    "be valid for reference sudoku" in assert(sudokuSolved.isSolved)
  }
  val base = Paths.get("src/test/resources/net/ladstatt/sudoku/testdata/cells/")


  /** with my current testdata, i don't achieve 100% recognition rate for every number ... :/  */
  "SCell exact image recognition" should {
    "recognize exactly 1" in detectExactly(base, 1)
    "recognize exactly 2" in detectExactly(base, 2)
    "recognize exactly 3" in detectExactly(base, 3)
    "recognize exactly 4" in detectExactly(base, 4)
    "recognize exactly 5" in detectExactly(base, 5)
    "recognize exactly 6" in detectExactly(base, 6)
    "recognize exactly 7" in detectExactly(base, 7)
    "recognize exactly 8" in detectExactly(base, 8)
    "recognize exactly 9" in detectExactly(base, 9)
  }

  "Scell overall image recognition" should {
    "recognize 1 overall" in detectOverall(base, 1)
    "recognize 2 overall" in detectOverall(base, 2)
    "recognize 3 overall" in detectOverall(base, 3)
    "recognize 4 overall" in detectOverall(base, 4)
    "recognize 5 overall" in detectOverall(base, 5)
    "recognize 6 overall" in detectOverall(base, 6)
    "recognize 7 overall" in detectOverall(base, 7)
    "recognize 8 overall" in detectOverall(base, 8)
    "recognize 9 overall" in detectOverall(base, 9)
  }

  private def detectExactly(base: Path, number: Int) = {
    val cells: scala.Seq[_root_.net.ladstatt.sudoku.SCell] = detect(base, number)
    groupAndPrint(cells)
    /*
    println("***")
    for {(c, hitlist) <- cells.groupBy(s => s.value)
         hit <- hitlist
         } {
      println(s"$c : ${hit.quality}")
    }
    groupAndPrint(cells.filter(_.value == number))
    */
    assert(cells.forall(c => c.detectedValue == number || c.detectedValue == 0))
  }

  def fromPath(p: Path, frameNr: Int, pos: Int): SCell = {
    SCell(p.getFileName.toString, frameNr, pos, JavaCV.loadMat(p), new Rect(), Map().withDefaultValue(0))
  }

  private def detect(base: Path, number: Int): Seq[SCell] = {
    val files = Files.list(base.resolve(number.toString)).iterator.asScala.toSeq
    val cells: Seq[SCell] =
      (for ((p, i) <- files.zipWithIndex) yield {
        // println(p.getFileName.toString)
        val s = fromPath(p, 0, i)
        if (s.detectedValue != number) {
          logError(s"${p.toAbsolutePath.toString} should be recognized as $number but was ${s.detectedValue}")
        }
        s
      })
    cells
  }

  /** tests that given more than one image, the image recognition yields expectedNumber overall */
  def detectOverall(base: Path, expectedNumber: Int): Assertion = {
    val files = Files.list(base.resolve(expectedNumber.toString)).iterator.asScala.toSeq
    //    files.foreach(println)
    //   println(files)
    val res: Map[Int, Int] =
    files.zipWithIndex.foldLeft(Map[Int, Int]().withDefaultValue(0)) {
      case (acc, (p, i)) =>
        SCell(p.getFileName.toString, 0, i, JavaCV.loadMat(p), new Rect(), acc).hits
    }
    println(res)
    val (detectedNumber, _) =
      res.toSeq.sortWith {
        case (a, b) => a._2 > b._2
      }.head
    assert(expectedNumber == detectedNumber)
  }

  private def groupAndPrint(cells: Seq[SCell]): Unit = {
    val grouped: Map[Int, Seq[SCell]] = cells.groupBy(s => s.detectedValue)
    grouped.foreach {
      case (value, hits) => println(s"$value -> ${hits.size}")
    }
  }

  "SudokuCanvas" should {
    /**
     * given an input frame, detectSudoku canvas
     */
    "be detected correctly" in {
      for (env <- validSudokus) {
        env.optSudoku match {
          case Some(sudoku) =>
            // JavaCV.writeMat(Paths.get(s"target/${env.id}-normalized.png"), sudoku.normalized)
            assert(sudoku.corners == env.corners)
            assert(sudoku.cells.size == 81)
          case None => fail("Could not detect sudoku canvas")
        }
      }
    }


    def solve(toSolve: SudokuEnvironment, onSuccess: SolvedSudoku => Unit): Unit = {
      toSolve.optSudoku match {
        case Some(value) =>
          value.trySolve match {
            case Some(value) => onSuccess(value)
            case None =>
              fail()
          }
        case None =>
          fail()
      }
    }

    "create solved image" in {
      solve(sudoku1ReadyToSolve, {
        solvedSudoku =>
          solvedSudoku.optCNormalized match {
            case Some(value) =>
              JavaCV.writeMat(Sudoku.targetPath.resolve("cNormalized.png"), value)
            case None =>
              fail()
          }
      })
    }
    "solve exact number at ease" in {
      solve(sudoku1ReadyToSolve, {
        value =>
          assert(value.sudokuHistory.isSolved)
          assert(value.sudokuHistory == Sudokus.sudokuSolved)
      })
    }

    "why does warp feel so really bad" in {

      //val m: Mat = JavaCV.loadMat(Paths.get("/Users/lad/Documents/sudokufx/sudoku-core/src/test/resources/net/ladstatt/sudoku/testdata/frame1.png"))
      for (i <- 1 to 100) {
        val srcCorners = new Mat(new Size(2, 4), opencv_core.CV_32F, new FloatPointer(862f, 283f, 1240f, 339f, 1172f, 711f, 804f, 640f))
        val transformationMatrix = opencv_imgproc.getPerspectiveTransform(srcCorners, JavaCV.mkCorners(Parameters.size1280x720.width, Parameters.size1280x720.height))
        val m: Mat = JavaCV.loadMat(getClass, MatCp("/net/ladstatt/sudoku/testdata/frame1.png"))
        val res = new Mat()
        opencv_imgproc.warpPerspective(m, res, transformationMatrix, Parameters.size1280x720)
        if ((i % 100) == 0) {
          JavaCV.writeMat(Sudoku.targetPath.resolve(s"$i-warped.png"), res)
        }
      }
    }

    "why does warp feel so bad" in {
      for (i <- 1 to 100) {
        val fp = new FloatPointer(862, 283, 1240, 339, 1172, 711, 804, 640)
        val srcCorners = new Mat(new Size(2, 4), opencv_core.CV_32F, fp)
        val transformationMatrix = opencv_imgproc.getPerspectiveTransform(srcCorners, Parameters.normalizedCorners)
        val m: Mat = JavaCV.loadMat(getClass, MatCp("/net/ladstatt/sudoku/testdata/frame1.png"))
        val res = JavaCV.warpP(m, transformationMatrix)
        if ((i % 25) == 0) {
          JavaCV.writeMat(Sudoku.targetPath.resolve(s"$i-warped.png"), res)
        }
      }
    }
    "find and solve a sudoku for a single frame n times applied" in {
      val envs: Seq[SudokuEnvironment] =
        for (i <- 0 to 100) yield sudoku1Empty.copy(frameNr = i)

      val envComputed =
        envs.foldLeft(sudoku1Empty) {
          case (acc, env) =>
            // println("!!!!!!!!!!!!!!")
            env.copy(history = acc.history).optSudoku match {
              case None =>
                ???
              case Some(s) =>
                acc.copy(history = s.history)
            }
        }
      logInfo("\n" + envComputed.history.asSudokuString)
      // now we have enough info to try to solve sudoku
      envComputed.optSudoku match {
        case Some(s) =>
          s.trySolve match {
            case None => fail("Could not solve " + envComputed.id + " successfully.")
            case Some(r) =>
              // yeS!
              println(r.sudokuHistory.asSudokuString)
          }
        case None => fail()
      }
      println(envComputed.history)

    }
  }


}
