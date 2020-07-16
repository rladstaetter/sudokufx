package net.ladstatt.sudoku

import java.nio.IntBuffer


object Label {

  def apply(i: Int, ibf: IntBuffer): Label = {
    val a = Array(0, 0, 0, 0, 0)
    ibf.get(a)
    apply(i, a)
  }

  def apply(i: Int, a: Array[Int]): Label = {
    assert(a.length == 5)
    val Array(x, y, w, h, area) = a
    Label(i, x, y, w, h, area)
  }

}

case class Label(i: Int
                 , x: Int
                 , y: Int
                 , w: Int
                 , h: Int
                 , a: Int) {

  def touches(width: Int, height: Int): Boolean = {
    x <= width * 0.1 || y == height * 0.1 || x + w >= 0.9 * width || y + h >= 0.9 * height
  }
}