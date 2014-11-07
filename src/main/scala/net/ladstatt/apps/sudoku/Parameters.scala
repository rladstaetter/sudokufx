package net.ladstatt.apps.sudoku

import net.ladstatt.opencv.OpenCV
import org.opencv.core.{Mat, Size}

import scala.io.Source

/**
 * Created by lad on 26.10.14.
 */
object Parameters {

  val ssize = 9
  val cellCount = ssize * ssize

  val range = 0 until ssize
  val digitRange = 0 to ssize

  val cellRange = 0 until cellCount

  val colorRange = 0 to 256 by 16
  private val leftRange: Seq[Int] = Seq(0, 1, 2)
  private val middleRange: Seq[Int] = Seq(3, 4, 5)
  private val rightRange: Seq[Int] = Seq(6, 7, 8)
  val sectors: Seq[Seq[Int]] = Seq(leftRange, middleRange, rightRange)

  val (templateWidth, templateHeight) = (25.0, 50.0)
  val templateSize = new Size(templateWidth, templateHeight)
  lazy val sudokuSize = new Size(templateWidth * ssize, templateHeight * ssize)

  lazy val templateCorners = OpenCV.mkCorners(sudokuSize)


  lazy val templateLibrary: Map[Int, Mat] = {
    val digits: Seq[Array[Int]] =
      (Source.fromInputStream(getClass.getResourceAsStream("/net/ladstatt/apps/sudokufx/templates.csv")).getLines.map(l => l.split(",").map(e => if (e == "0") 0 else 255))).toSeq

    (1 to 9).map {
      case i => i -> OpenCV.toMat(digits(i - 1), Parameters.templateSize)
    }.toMap
  }


  def row(i: SIndex): Int = i / 9

  def col(i: SIndex): Int = i % 9
}
