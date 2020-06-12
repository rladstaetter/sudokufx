package net.ladstatt.sudoku

import net.ladstatt.sudoku.JavaCV._
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{Mat, Point, Range, Rect}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


/**
 * Represents a cell of the sudoku.
 *
 * A sudoku contains 81 cells. Every cell has its proposed value, a region of interest and some other
 * attributes.
 *
 * @param cellMat video input for this specific area
 * @param roi     region of interest; coordinates for this cell
 */
case class SCell(cellMat: Mat, roi: Rect) {


  val cell: Mat = FramePipeline.toGray(cellMat)

  // only search for contours in a subrange of the original cell to get rid of possible border lines
  val (width, height) = (cell.size.width, cell.size.height)
  val cellData = new Mat(cell, new Range((height * 0.1).toInt, (height * 0.9).toInt), new Range((width * 0.1).toInt, (width * 0.9).toInt))
  val cellArea = cellData.size().area
  val (minArea, maxArea) = (0.15 * cellArea, 0.5 * cellArea)
  val (centerX, centerY) = (cellData.size.width / 2, cellData.size.height / 2)
  val sp = SpecialParam(new Point(centerX, centerY), minArea, maxArea)

  val equalized = equalizeHist(cellData)

  val blurred = FramePipeline.gaussianblur(equalized)
  val thresholded = threshold(blurred)
  val a = FramePipeline.bitwiseNot(thresholded)
  val contour: Option[Mat] = findCellContour(a, sp, opencv_imgproc.RETR_TREE, opencv_imgproc.CHAIN_APPROX_SIMPLE)

  val computValueAndQuality: Future[(Int, Double)] = contour.map(TemplateLibrary.detectNumber).getOrElse(Future.successful((0, 0.0)))

  val (value, quality) = Await.result(computValueAndQuality, Duration.Inf)


  /*
  val (width, height) = (cellMat.size.width, cellMat.size.height)
  val (centerX, centerY) = (width / 2, height / 2)
  val cellData = new Mat(cell, new Range((height * 0.1).toInt, (height * 0.9).toInt), new Range((width * 0.1).toInt, (width * 0.9).toInt))
  val cellArea = cellData.size().area
  val (minArea, maxArea) = (0.15 * cellArea, 0.5 * cellArea)
  val sParam = SpecialParam(new Point(centerX, centerY), minArea, maxArea)
  val equalized = equalizeHist(cellData)
  val blurred = FramePipeline.gaussianblur(equalized)
  val thresholded = threshold(blurred)
  val bitnotted = FramePipeline.bitwiseNot(thresholded)
  val someContour: Option[Mat] = findCellContour(bitnotted, sParam, opencv_imgproc.RETR_TREE, opencv_imgproc.CHAIN_APPROX_SIMPLE)

  val computeValueAndQuality: Future[(Int, Double)] = someContour.map(TemplateLibrary.detectNumber).getOrElse(Future.successful((0, 0.0)))

  val (value, quality) = Await.result(computeValueAndQuality, Duration.Inf)

   */
}
