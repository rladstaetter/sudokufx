package net.ladstatt.sudoku

import java.util

import net.ladstatt.opencv.OpenCV
import org.opencv.core.Point
import org.scalatest.WordSpecLike


/**
 * Created by lad on 22.02.16.
 */
class SudokuUtilsSpec extends WordSpecLike {

  OpenCV.loadNativeLib()

  "detectCorners" ignore {
    val params: SParams = SParams()
    val pipeline: FramePipeline = FramePipeline(SudokuTestContext.frameSudoku_1, params)
    val res = SudokuUtils.detectRectangle(pipeline.dilated, OpenCV.mkCorners(pipeline.dilated.size), params, pipeline.contours)
    //pipeline.persist(new File("target/utilstest/"))
    val points: util.List[Point] = res.get.toList
    assert(points.size > 0)
    import scala.collection.JavaConverters._
    val pts: Seq[Point] = res.map(_.toList.asScala.toSeq).getOrElse(Seq())
    assert(
      Seq(
        new Point(231.0, 175.0),
        new Point(677.0, 157.0),
        new Point(709.0, 583.0),
        new Point(265.0, 624.0)) == pts)

  }

}
