package net.ladstatt.opencv

import java.io.File

import net.ladstatt.sudoku.SCandidate
import org.bytedeco.opencv.opencv_core.Mat
import org.bytedeco.opencv.global.opencv_imgcodecs._

object Debug {

  val Active = false

  def writeDebug(sCandidate: SCandidate) = {
    val parent = new File(s"/Users/lad/Documents/sudokufx/target/${sCandidate.nr}")
    parent.mkdirs()
    //println("InputCorners:" + sCandidate.framePipeline.corners)
    //println("CellWidth  :" + sCandidate.sRectangle.cellWidth + "/" + sCandidate.sRectangle.cellHeight)
    // write sudoku canvas
    sCandidate.pipeline.persist(parent)
    writeMat(new File(parent, s"normalized"), sCandidate.sRectangle.normalized)
  }

  def writeMat(target: File, mat: Mat): Boolean = {
    println(target)
    imwrite(target.getAbsolutePath + ".png", mat)
  }


}
