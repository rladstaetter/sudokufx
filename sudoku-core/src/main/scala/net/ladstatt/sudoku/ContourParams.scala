package net.ladstatt.sudoku

import org.bytedeco.opencv.global.opencv_imgproc


object ContourParams {

  def apply(): ContourParams = {
    ContourParams(opencv_imgproc.RETR_TREE, opencv_imgproc.CHAIN_APPROX_SIMPLE, 30)
  }

}


/**
 * Configures various aspects of the sudoku detection algorithm.
 *
 * @param retrivalMode
 * @param approximation
 * @param contourRatio
 */
case class ContourParams(retrivalMode: Int,
                         approximation: Int,
                         contourRatio: Int)
