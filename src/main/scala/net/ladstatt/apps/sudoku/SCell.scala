package net.ladstatt.apps.sudoku

import org.opencv.core.Rect


case class SCell(value: Int, quality: Double, roi: Rect) {
  assert(0 <= value && value <= 9, s"value: $value")
  assert(quality >= 0)
}
