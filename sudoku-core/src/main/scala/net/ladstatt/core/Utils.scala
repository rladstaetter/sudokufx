package net.ladstatt.core


object Utils  {

  /**
    * function to measure execution time of first function, optionally executing a display function,
    * returning the time in ms
    */
  def time[A](a: => A, display: Long => Unit): A = {
    val before = System.nanoTime
    val result = a
    val millis = (System.nanoTime - before) / 1000000
    display(millis)
    result
  }


}

