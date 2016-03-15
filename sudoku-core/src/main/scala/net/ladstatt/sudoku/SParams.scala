package net.ladstatt.sudoku

import org.opencv.imgproc.Imgproc


object SParams {

  def apply(): SParams = {
    SParams(Imgproc.RETR_TREE, Imgproc.CHAIN_APPROX_SIMPLE, 30)
  }
}


/**
  * Configures various aspects of the sudoku detection algorithm.
  *
  * @param contourMode
  * @param contourMethod
  * @param contourRatio
  */
case class SParams(contourMode: Int,
                   contourMethod: Int,
                   contourRatio: Int)
