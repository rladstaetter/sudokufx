package net.ladstatt.sudoku

import net.ladstatt.opencv.OpenCV
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by lad on 01.05.16.
  */
object SRectangle {

  def apply(fp: FramePipeline): Option[SRectangle] = {
    fp.detectedRectangle.map(new SRectangle(fp.frame, _, fp.corners))
  }
}

case class SRectangle(frame: Mat, detectedCorners: MatOfPoint2f, destCorners: MatOfPoint2f) {

  //val analysisCorners = OpenCV.mkCorners(TemplateLibrary.templateCanvasSize)
  val normalized: Mat = OpenCV.warp(frame, detectedCorners, destCorners)
  val warpedCellSize: Size = OpenCV.mkCellSize(normalized.size)
  /**
    * the cellRois denote the region of interests for every sudoku cell (there are 81 of them for every sudoku)
    */
  val cellRois: Seq[Rect] = Parameters.cellRange.map(OpenCV.mkRect(_, warpedCellSize))

  val cells: Seq[SCell] = cellRois.map(r => SCell(normalized.submat(r), r))

  // lazy val detectedCells: Future[Seq[SCell]] = Future.fold(warpedCells)(Seq[SCell]())((cells, c) => cells ++ Seq(c))

  /**
    * paints the solution to the canvas.
    *
    * returns the modified canvas with the solution painted upon.
    *
    * detectedCells contains values from 0 to 9, with 0 being the cells which are 'empty' and thus have to be filled up
    * with numbers.
    *
    * uses digitData as lookup table to paint onto the canvas, thus modifying the canvas.
    */
  def paintSolution(detectedCells: Seq[Int],
                    someSolution: Option[Cells],
                    digitLibrary: DigitLibrary): Future[Mat] = {

    Future {
      for (solution <- someSolution) {
        val values: Array[Int] = solution.map(_.value)
        for ((s, r) <- values zip cellRois) {
          if (values.sum == 405) {
            copyTo(digitLibrary(s)._2.getOrElse(SudokuUtils.mkFallback(s, digitLibrary).get), normalized, r)
          } else {
            logTrace("values.sum was not 405 in paintsolution")
          }
        }
      }
      normalized
    }
  }


}