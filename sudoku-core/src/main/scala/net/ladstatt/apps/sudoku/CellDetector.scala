package net.ladstatt.apps.sudoku

import net.ladstatt.apps.sudoku.Parameters._
import net.ladstatt.opencv.OpenCV._
import org.opencv.core.Rect
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by lad on 02.02.15.
 */
case class CellDetector(sudokuCanvas: VideoInput) {

  val cellSize = mkCellSize(sudokuCanvas.size)
  val cellRects: Seq[Rect] = cellRange.map(mkRect(_, cellSize))
  val futureSCells: Seq[Future[SCell]] = cellRects.map(detectCell(TemplateLibrary.detectNumber,sudokuCanvas, _))

  // 81 possibly detected cells, most of them probably filled with 0's
  val futureDetectedCells: Future[Seq[SCell]] = Future.fold(futureSCells)(Seq[SCell]())((cells, c) => cells ++ Seq(c))
}
