package net.ladstatt.apps.sudoku

import org.opencv.core.Mat

/**
 * Created by lad on 26.10.14.
 */
// TODO remove: replace with Arrays in SudokuState
// digitSolutionData : Array[Mat]
// digitSolutionQuality : Array[Double]
case class SCell(value: Int, quality: Double, data: Mat) {
  assert(0 <= value && value <= 9, s"value: $value")
  assert(quality >= 0)

}
