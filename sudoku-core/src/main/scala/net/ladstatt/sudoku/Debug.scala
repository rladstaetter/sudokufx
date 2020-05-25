package net.ladstatt.sudoku

import java.io.File

import org.bytedeco.opencv.global.opencv_imgcodecs._
import org.bytedeco.opencv.opencv_core.Mat

object Debug {

  val Active = false

  def writeDebug(sCandidate: SCandidate): Boolean = {
    val parent = new File(s"/Users/lad/Documents/sudokufx/target/${sCandidate.nr}")
    parent.mkdirs()
    //println("InputCorners:" + sCandidate.framePipeline.corners)
    //println("CellWidth  :" + sCandidate.sRectangle.cellWidth + "/" + sCandidate.sRectangle.cellHeight)
    // write sudoku canvas
    sCandidate.pipeline.persist(parent)
    writeMat(new File(parent, s"normalized"), sCandidate.sRectangle.normalized)
  }

  def writeMat(target: File, mat: Mat): Boolean = {
    imwrite(target.getAbsolutePath + ".png", mat)
  }


}
