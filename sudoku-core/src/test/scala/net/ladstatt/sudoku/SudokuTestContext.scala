package net.ladstatt.sudoku

import scala.io.Source

/**
  * Created by lad on 06.01.16.
  */
object SudokuTestContext {

  // see http://norvig.com/easy50.txt
  // and also make sure to visit http://norvig.com/sudoku.html
  val easySudokus =
    Source.fromInputStream(getClass.getResourceAsStream("easysudokus.txt")).getLines().mkString("\n")

}
