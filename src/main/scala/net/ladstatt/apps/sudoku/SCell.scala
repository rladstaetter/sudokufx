package net.ladstatt.apps.sudoku

import net.ladstatt.opencv.OpenCV
import org.opencv.core.Mat


object SCell {

  def apply(value: Int, quality: Double, data: Mat): SCell = {
    new SCell(value, quality, data)
  }

}

/**
 * Created by lad on 26.10.14.
 */
// TODO remove Mat parameter, replace whole datastructure with Array[(value,quality)]
class SCell(val value: Int, val quality: Double, val data: Mat) {
  assert(0 <= value && value <= 9, s"value: $value")
  assert(quality >= 0)

  def duplicate(): SCell = {
    SCell(value, quality, OpenCV.copyMat(data))
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: SCell => (that canEqual (this)) &&
        (this.value == that.value) &&
        (this.quality == that.quality) &&
        (this.data == that.data)
      case _ => false
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[SCell]

  override def hashCode(): Int = {
    (41 * (41 + value) + quality + data.nativeObj).toInt
  }

  override def toString(): String = s"SCell($value,$quality,<Native:${data.nativeObj}>)"

}
