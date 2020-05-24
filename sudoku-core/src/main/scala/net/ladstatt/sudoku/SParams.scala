package net.ladstatt.sudoku

import org.bytedeco.opencv.global.opencv_imgproc


object SParams {

  def apply(): SParams = {
    SParams(opencv_imgproc.RETR_TREE, opencv_imgproc.CHAIN_APPROX_SIMPLE, 30)
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
