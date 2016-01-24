package net.ladstatt.sudoku

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, File}
import javafx.embed.swing.SwingFXUtils
import javafx.scene.image.Image

import net.ladstatt.core.CanLog
import org.opencv.core.{Mat, MatOfByte, Point}
import org.opencv.imgcodecs.Imgcodecs

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait OpenCVJfxUtils extends CanLog {

   // TODO profile if toImage or toImage2 is faster
  def toImage(mat: Mat): Image =  {
    val byteMat = new MatOfByte()
    Imgcodecs.imencode(".bmp", mat, byteMat)
    new Image(new ByteArrayInputStream(byteMat.toArray))
  }

  def toImage2(matrix: Mat): Image = logWithTimer("frame2",{
    val cols = matrix.cols()
    val rows = matrix.rows()
    val elemSize = matrix.elemSize()
    val data: Array[Byte] = new Array[Byte](cols * rows * elemSize.toInt)
    matrix.get(0, 0, data)

    val lType = matrix.channels() match {
      case 1 => BufferedImage.TYPE_BYTE_GRAY
      case 3 => BufferedImage.TYPE_3BYTE_BGR
      case 4 => BufferedImage.TYPE_4BYTE_ABGR
      case _ => BufferedImage.TYPE_BYTE_GRAY
    }

    matrix.channels() match {
      case 3 =>
        var i = 0
        while (i < data.length) {
          val b = data(i)
          data(i) = data(i + 2)
          data(i + 2) = b
          i = i + 3
        }
      case 4 =>
        var i = 0
        while (i < data.length) {
          val b = data(i)
          data(i) = data(i + 2)
          data(i + 2) = b
          i = i + 4
        }
      case _ =>
    }

    val image = new BufferedImage(cols, rows, lType)
    image.getRaster.setDataElements(0, 0, cols, rows, data)
    SwingFXUtils.toFXImage(image, null)
  }                   )


  def convert2PolyLinePoints(points: Iterable[Point]): List[java.lang.Double] = {
    if (points.isEmpty)
      List()
    else {
      val ps = points.flatMap(p => List[java.lang.Double](p.x, p.y)).toList
      ps ++ List(ps.head, ps(1))
    }
  }

  def loadImage(file: File): Future[Mat] = Future {
    Imgcodecs.imread(file.getAbsolutePath)
  }

}
