package net.ladstatt.sudoku

import net.ladstatt.opencv.OpenCV._
import org.opencv.core.{Mat, Rect}

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

  val (value, quality) = Await.result(contour.map(TemplateLibrary.detectNumber).getOrElse(Future.successful((0, 0.0))),Duration.Inf)
}
