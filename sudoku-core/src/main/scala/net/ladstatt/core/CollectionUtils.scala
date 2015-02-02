package net.ladstatt.core

/**
 * Created by lad on 02.02.15.
 */
object CollectionUtils {

  def traverseWithIndex[A](cs: Seq[A])(up: (A, Int) => Unit): Unit = {
    var i = 0
    for (c <- cs) {
      up(c, i)
      i = i + 1
    }
  }
}
