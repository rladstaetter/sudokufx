package net.ladstatt.apps.sudoku

import java.io.File

import net.ladstatt.core.Utils
import org.junit.Assert._
import org.junit.Test

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.{Success, Try}


/**
 * Created by lad on 05.05.14.
 */
class SudokuTest extends OpenCvUnitTest with Utils {
  val sudoku69 = SCandidate(0, frame69, 1, 17)

  val refCellNumbers: Seq[(Int, Double)] = {
    val lines: Iterator[String] = Source.fromFile(new File("src/test/resources/cellNumbers69.csv")).getLines
    (for (l <- lines) yield {
      val a = l.split(',')
      (a(0).toInt, a(1).toDouble)
    }).toSeq
  }


  // compares individual detection results with a reference file
  @Test def testDetect(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    assertEquals(81.toLong, refCellNumbers.size.toLong)
    val cells = Await.result(Future.sequence(sudoku69.cellNumbers), Duration.Inf)
    var i = 0
    for (c <- cells) {
      assertEquals(refCellNumbers(i)._1.toLong, c.value.toLong)
      assertEquals(refCellNumbers(i)._2, c.quality, 0.000001D)
      i = i + 1
    }
  }

  @Test def testSudoku69(): Unit = {

    Try {
      Await.result(sudoku69.calc, Duration.Inf)
    } match {
      case Success(s: SSuccess) => {
        assertEquals( """617948532
                        |524361879
                        |389725641
                        |742816953
                        |156439728
                        |938257416
                        |275683194
                        |461592387
                        |893174265""".stripMargin, s.solutionAsString)
      }
      case Success(SFailure(c)) => {
        fail(c.statsAsString())
      }
      case _ => ???
    }

  }
}
