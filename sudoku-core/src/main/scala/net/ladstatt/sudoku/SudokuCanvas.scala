package net.ladstatt.sudoku

import java.nio.FloatBuffer

import net.ladstatt.sudoku.JavaCV._
import org.bytedeco.opencv.opencv_core.{Mat, Rect}

object SudokuCanvas {
  /*
    def apply(pipeline: FramePipeline) : SRectangle = {
      SRectangle(pipeline.frame, pipeline.detectRectangle.get,pipeline.corners)
    }

   */
}

/**
 * Created by lad on 01.05.16.
 *
 * @param inputFrame      video input
 * @param detectedCorners corner points of detected sudoku
 */
case class SudokuCanvas(inputFrame: Mat
                        , detectedCorners: Mat) {

  /**
   * This mat contains an 'unstretched' version of the detected sudoku outer rectangle.
   *
   * In this representation it is easier to paint upon. After painting this Mat will be retransformed
   * to the original appearance again.
   */
  val normalized: Mat = JavaCV.warp(inputFrame, detectedCorners)

  /**
   * the cellRois denote the region of interests for every sudoku cell (there are 81 of them for every sudoku)
   */
  val cellRois: Seq[Rect] = Parameters.cellRange.map(JavaCV.mkRect(_, JavaCV.mkCellSize(normalized.size)))

  val cells: Seq[SCell] = cellRois.map(r => SCell(normalized.apply(r), r))

  val cellValues: Seq[Int] = cells.map(_.value)

  /** an ascii 2D representation of the sudoku */
  lazy val sudokuConfiguration: SudokuConfiguration =
    SudokuConfiguration(
      (for (line <- cellValues.sliding(Parameters.ssize, Parameters.ssize)) yield
        line.mkString("")
        ).mkString("\n"))

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
        val normalizedCell: Mat = digitLibrary(s)._2.getOrElse(SudokuUtils.mkFallback(s, digitLibrary).get)

        copyTo(normalizedCell, normalized, r)
      }
    }
    normalized
  }




  /* sudoku outer rectangle */
  def corners: Seq[Float] = {
    val bf = detectedCorners.createBuffer[FloatBuffer]
    val (x1, y1) = (bf.get(0), bf.get(1))
    val (x2, y2) = (bf.get(2), bf.get(3))
    val (x3, y3) = (bf.get(4), bf.get(5))
    val (x4, y4) = (bf.get(6), bf.get(7))
    Seq(x1, y1
      , x2, y2
      , x3, y3
      , x4, y4)
  }


}