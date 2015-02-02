package net.ladstatt.core

/**
 * Created by lad on 02.02.15.
 */
trait Timed {
  /**
   * function to measure execution time of first function, optionally executing a display function,
   * returning the time in ms
   */
  def time[A](a: => A, display: Long => Unit = s => ()): A = {
    val before = System.nanoTime
    val result = a
    val millis = (System.nanoTime - before) / 1000
    display(millis)
    result
  }

}
