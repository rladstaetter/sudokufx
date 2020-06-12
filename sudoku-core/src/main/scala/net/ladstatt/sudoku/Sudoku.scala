package net.ladstatt.sudoku

import java.nio.file.Path

import org.bytedeco.opencv.opencv_core.Mat

case class SudokuConfiguration(cells: String)

object Sudoku {
  /**
   * provide path to frame image and expected solution
   *
   * @param path                path to frame
   * @param sudokuConfiguration encoded in 81 chars, 0 means empty
   * @return
   */
  def apply(id: String
            , path: Path
            , sudokuConfiguration: SudokuConfiguration
            , corners: Seq[Float]): Sudoku = {
    Sudoku(id, JavaCV.mkMat(path), sudokuConfiguration, corners)
  }
}

case class Sudoku(id: String
                  , frame: Mat
                  , sudokuConfiguration: SudokuConfiguration
                  , corners: Seq[Float]
                  , contourParams: ContourParams = ContourParams()) {


  lazy val pipeline = FramePipeline(frame)
}