package net.ladstatt.sudoku

import net.ladstatt.opencv.OpenCV
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._

import scala.collection.JavaConversions ._

object SRectangle {

  def apply(pipeline: FramePipeline) : SRectangle = {
    SRectangle(pipeline.frame, pipeline.detectRectangle.get,pipeline.corners)
  }
}
/**
  * Created by lad on 01.05.16.
  */
case class SRectangle(frame: Mat, detectedCorners: MatOfPoint2f, destCorners: MatOfPoint2f) {
  /**
    * This mat contains an 'unstretched' version of the detected sudoku outer rectangle.
    *
    * In this representation it is easier to paint upon. After painting this Mat will be retransformed
    * to the original appearance again.
    */
  val normalized: Mat = OpenCV.warp(frame, detectedCorners, destCorners)

  /**
    * the cellRois denote the region of interests for every sudoku cell (there are 81 of them for every sudoku)
    */
  val cellRois: Seq[Rect] = Parameters.cellRange.map(OpenCV.mkRect(_, OpenCV.mkCellSize(normalized.size)))

  val cells: Seq[SCell] = cellRois.map(r => SCell(normalized.submat(r), r))

  val cellValues: Seq[Int] = cells.map(_.value)

  lazy val detectedCells: Seq[SCell] = cells.filter(_.value != 0)

  lazy val corners = detectedCorners.toList.toList
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
  def paintSolution(someSolution: Option[Cells],
                    digitLibrary: DigitLibrary): Mat = {
    for (solution <- someSolution) {
      val values: Array[Int] = solution.map(_.value)
      for ((s, r) <- values zip cellRois if s != 0) {
        copyTo(digitLibrary(s)._2.getOrElse(SudokuUtils.mkFallback(s, digitLibrary).get), normalized, r)
      }
    }
    normalized
  }


}