package net.ladstatt.apps.sudoku

import net.ladstatt.opencv.OpenCV._
import org.opencv.core.{Mat, MatOfPoint2f}

/**
 * Created by lad on 02.02.15.
 */
case class Warper(frame: Mat, destCorners: MatOfPoint2f) {

  val sudokuCanvas: SudokuCanvas = warp(frame, destCorners, mkCorners(frame.size))

}
