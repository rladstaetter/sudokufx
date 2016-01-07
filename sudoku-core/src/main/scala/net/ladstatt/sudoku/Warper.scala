package net.ladstatt.sudoku

import net.ladstatt.opencv.OpenCV
import org.opencv.core.{Mat, MatOfPoint2f}

/**
  * Created by lad on 02.02.15.
  */
case class Warper(frame: Mat, destCorners: MatOfPoint2f) {

  val sudokuCanvas: Mat = OpenCV.warp(frame, destCorners, OpenCV.mkCorners(frame.size))

}
