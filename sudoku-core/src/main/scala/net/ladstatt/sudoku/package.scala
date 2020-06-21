package net.ladstatt


import org.bytedeco.opencv.opencv_core.Mat
import scala.concurrent.Future

/**
  * Created by lad on 29.04.14.
  */
package object sudoku {

  /**
    * records for each number from 0 to 9 the best hit (quality) along with its digital data
    */
  type DigitLibrary = Map[Int, (Double, Option[Mat])]


  // represents the computed solution
  type SudokuDigitSolution = Array[Char]

}
