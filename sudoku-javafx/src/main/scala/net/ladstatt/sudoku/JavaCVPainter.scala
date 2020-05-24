package net.ladstatt.sudoku

import java.nio.ByteBuffer

import javafx.scene.image._
import org.bytedeco.opencv.global._
import org.bytedeco.opencv.global.opencv_imgproc.COLOR_BGR2BGRA
import org.bytedeco.opencv.opencv_core.Mat

object JavaCVPainter {

  val javaCVMat = new Mat

  /** has to be lazy otherwise nullpointer is thrown at startup */
  lazy val buffer: ByteBuffer = javaCVMat.createBuffer()
  val formatByte: WritablePixelFormat[ByteBuffer] = PixelFormat.getByteBgraPreInstance

  def toImage(mat: Mat): WritableImage = {
    opencv_imgproc.cvtColor(mat, javaCVMat, COLOR_BGR2BGRA)
    val pb = new PixelBuffer(mat.size().width, mat.size.height, buffer, formatByte)
    new WritableImage(pb)
  }

}
