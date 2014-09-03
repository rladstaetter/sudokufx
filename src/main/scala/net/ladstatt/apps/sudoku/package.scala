package net.ladstatt.apps

import org.opencv.core.Mat
import scala.concurrent.Future

/**
 * Created by lad on 29.04.14.
 */
package object sudoku {

  type Pos = (Int, Int)
  type SNum = Int
  type SCount = Int
  type SHitQuality = Double
  //type Frequency = Map[SNum, SCount]
  type Frequency = Array[SCount]
  type DetectionMethod = Option[Mat] => Future[(SNum, SHitQuality)]

  // TODO solution should be an array of Int or a string with 81 entries
  // the graphical representation should be a single mat with the matrixes edited inline
  // (meaning the matrix represents the whole canvas and the app is copying submatrixes
  type Cells = Map[Pos, SCell]
}
