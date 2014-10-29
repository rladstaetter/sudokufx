package net.ladstatt.apps.sudoku

/**
 * Created by lad on 26.10.14.
 */
case class FrameSuccess(detectedCells: Cells,
                        digitSolution: Option[SudokuDigitSolution],
                        solutionCells: Option[Cells])
