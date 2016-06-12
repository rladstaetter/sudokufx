package net.ladstatt

import org.opencv.core.Mat

import scala.concurrent.Future

/**
  * Created by lad on 29.04.14.
  */
package object sudoku {

  //
  // contains 81 elements, on each position the number of hits for the digit is detected.
  // this means if for a cell 2 times a 3 is detected, the Map looks like this:
  // Map(... PosAtSudoku -> Map(3 -> 2) ...)
  //
  type HitCounters = Map[Int, Map[Int, Int]]

  // a function which returns a detected number and a quality thereof
  type DetectFn = Mat => Future[(Int, Double)]


  /**
    * records for each number from 0 to 9 the best hit (quality) along with its digital data
    */
  type DigitLibrary = Map[Int, (Double, Option[Mat])]

  // TODO solution should be an array of Int or a string with 81 entries
  // the graphical representation should be a single mat with the matrixes edited inline
  // (meaning the matrix represents the whole canvas and the app is working with submatrixes
  // and only saving the rect which is the region of interest, thus not creating intermediate mats
  type Cells = Array[SCell]

  // represents the computed solution
  type SudokuDigitSolution = Array[Char]

}
