package net.ladstatt.sudoku

import org.opencv.core.Rect


/**
  * Represents a cell of the sudoku.
  *
  * A sudoku contains 81 cells. Every cell has its proposed value, a region of interest and some other
  * attributes.
  *
  * @param value
  * @param quality
  * @param roi
  */
case class SCell(value: Int, quality: Double, roi: Rect) {
  assert(0 <= value && value <= 9, s"value: $value")
  assert(quality >= 0)
}
