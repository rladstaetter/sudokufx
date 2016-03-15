package net.ladstatt.sudoku

import org.junit.Assert._
import org.junit.Test
import org.opencv.core.Point

/**
  * Created by lad on 22.02.16.
  */
class SudokuUtilsTest {


  @Test def detectCorners(): Unit = {
    val pipeline: FramePipeline = FramePipeline(SudokuTestContext.frameSudoku_1)
    val (_, res) = SudokuUtils.detectSudokuCorners(pipeline.eroded, SParams())
    //pipeline.persist(new File("target/utilstest/"))
    assert(res.isDefined)
    assert(res.get.toList.size > 0)
    import scala.collection.JavaConversions._
    val pts: Seq[Point] = res.map(_.toList.toSeq).getOrElse(Seq())
    assertEquals(
      Seq(
        new Point(231.0, 175.0),
        new Point(677.0, 157.0),
        new Point(709.0, 583.0),
        new Point(265.0, 624.0)), pts)

  }

}
