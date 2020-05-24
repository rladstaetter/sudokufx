package net.ladstatt.sudoku

import java.io.File
import java.util.UUID

import net.ladstatt.opencv.JavaCV
import net.ladstatt.opencv.JavaCV._
import org.bytedeco.opencv.opencv_core.{Mat, Rect}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


/**
  * Represents a cell of the sudoku.
  *
  * A sudoku contains 81 cells. Every cell has its proposed value, a region of interest and some other
  * attributes.
  *
  * @param roi
  */
case class SCell(cellMat: Mat, roi: Rect) {

  val contour: Option[Mat] = Await.result(extractContour(cellMat), Duration.Inf)

  val computValueAndQuality: Future[(Int, Double)] = contour.map(TemplateLibrary.detectNumber).getOrElse(Future.successful((0, 0.0)))

  val (value, quality) = Await.result(computValueAndQuality, Duration.Inf)

  def persist(): Unit = {
    val libraryPath = new File(s"/Users/lad/Documents/sudokufx/sudoku-core/src/test/resources/net/ladstatt/sudoku/library/$value")
    libraryPath.mkdirs
    JavaCV.persist(cellMat, new File(libraryPath, UUID.randomUUID.toString + ".png"))
    ()
  }

}
