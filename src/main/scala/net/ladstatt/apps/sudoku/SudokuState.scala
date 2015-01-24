package net.ladstatt.apps.sudoku

import net.ladstatt.apps.sudoku.Parameters._
import net.ladstatt.apps.sudoku.SudokuAlgos._
import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

sealed trait SudokuResult

case class SCorners(nr: Int,
                    frame: Mat,
                    start: Long,
                    imageIOChain: ImageIOChain,
                    sudokuCanvas: SudokuCanvas,
                    detectedCells: Cells,
                    sudokuCorners: List[Point]) extends SudokuResult

case class SSuccess(nr: Int,
                    frame: Mat,
                    start: Long,
                    imageIOChain: ImageIOChain,
                    sudokuCanvas: SudokuCanvas,
                    foundCorners: Boolean,
                    detectedCells: Cells,
                    solution: SudokuDigitSolution,
                    solutionMat: Mat,
                    sudokuCorners: List[Point]) extends SudokuResult {

  def solutionAsString: String = solution.sliding(9, 9).map(new String(_)).mkString("\n")
}

case class SFailure(nr: Int, frame: Mat, start: Long, imageIoChain: ImageIOChain) extends SudokuResult

object SCandidate {

  def sectorIndizes(i: Int): Set[Int] = {
    val rowSector: Seq[Int] = sectors(row(i) / 3)
    val colSector: Seq[Int] = sectors(col(i) / 3)
    (for {r <- rowSector
          c <- colSector} yield r * 9 + c).toSet -- Set(i)
  }

  def isValid(hitCounts: HitCounts, values: Seq[Int], cap: Int): Boolean =
    values.zipWithIndex.forall {
      case (c, i) => SCandidate.posWellFormed(hitCounts, i, c, cap)
    }

  // searches rows and cols if there exist already the value in the same row or column
  private def rowColWellFormed(hitCounts: HitCounts, i: Int, value: Int, cap: Int): Boolean = {
    // val otherCells = cellRange.filter(u => u != i && ((row(u) == row(i) || col(u) == col(i)) || sectorIndizes(i).contains(u)))
    val otherCells = cellRange.filter(u => u != i && (row(u) == row(i) || col(u) == col(i)))
    !otherCells.exists(i => hitCounts(i).contains(value) && hitCounts(i)(value) == cap)
    // !otherCells.exists(i => hitCounts(i).contains(value))
  }

  def posWellFormed(hitCounts: HitCounts, i: SIndex, value: Int, cap: Int): Boolean = {
    value == 0 || rowColWellFormed(hitCounts, i, value, cap) //&& sectorWellFormed(hitCounts, i, value)
  }


}


/**
 * @param hCounts the individual detection numbers
 *                for each of the 81 sudoku cells, there exists a list which depicts how often a certain number
 *                was found in the sudoku, where the index in the list is the number (from 0 to 9, with 0 being
 *                the "empty" cell)

 * @param cap how often should a digit be detected before it is considered "stable enough"
 * @param minHits how many digits have to be detected before a solution attempt is executed
 */
case class SudokuState(hCounts: HitCounts = Array.fill(cellRange.size)(Array.fill[SCount](digitRange.size)(0)),
                       cap: Int = 8,
                       minHits: Int = 20,
                       cells: Seq[SCell] = Seq()) {

  def statsAsString(): String =
    s"""|${hitCountsAsString(hCounts)}
        |""".stripMargin

  def hitCountsAsString(hitCounts: HitCounts): String = {
    s"""Hitcounts:
       |----------
       |
       |${hitCounts.map(_.mkString(",")).mkString("\n")}
        |""".stripMargin
  }



  // TODO remove
  def countHits(cells: Seq[Int]): Unit = {

    def updateHitCounts(i: SIndex, value: Int): Unit = {
      val hitCountAtPos = hCounts(i)
      if (hitCountAtPos.max < cap) {
        hitCountAtPos(value) = 1 + hitCountAtPos(value)
      }
    }

    traverseWithIndex(cells)((c, i) => {
      if (SCandidate.posWellFormed(hCounts, i, c, cap)) {
        updateHitCounts(i, c)
      }
    })
  }


  /**
   * From a given list of cells, choose the ones which are the best hits
   * @param detectedCells
   * @return
   */
  def updateLibrary(digitLibrary: DigitLibrary,
                    detectedCells: Seq[SCell]): DigitLibrary = {
    val detectedValues: Seq[SIndex] = detectedCells.map(_.value)
    countHits(detectedValues)
    resetIfInvalidCellsDetected(digitLibrary, detectedValues)
  }

  def merge(sudokuCanvas: SudokuCanvas,
            digitLibrary: DigitLibrary,
            detectedCells: Seq[SCell]): DigitLibrary = time({

    /**
     * The filter returns only cells which contain 'better match' cells.
     *
     * If there are cells containing '0' detected they are ignored.
     */
    val qualityFilter: PartialFunction[SCell, Boolean] = {
      case c => (c.value != 0) && (c.quality < digitLibrary(c.value)._1) // lower means "better"
    }

    val hits: Seq[SCell] = detectedCells.filter(qualityFilter)
    val grouped: Map[Int, Seq[SCell]] = hits.groupBy(f => f.value)
    val optimal: Map[Int, SCell] = grouped.map { case (i, cells) => i -> cells.maxBy(c => c.quality)}
    // TODO add some sort of normalisation for each cell with such an effect that every cell has the same color 'tone'
    val tDL: DigitLibrary =
      (for (c <- optimal.values if digitLibrary(c.value)._1 > c.quality) yield {
        val newData = Some(copyMat(sudokuCanvas.submat(c.roi)))
        c.value -> ((c.quality, newData))
      }).toMap
    tDL
  }, t => logInfo(s"Merging took: ${t} micros"))


  def resetIfInvalidCellsDetected(digitLibrary: DigitLibrary, cells: Seq[Int]): DigitLibrary = {
    if (!SCandidate.isValid(hCounts, cells, cap)) {

      hCounts.transform(_ => Array.fill[SCount](Parameters.digitRange.size)(0))
      Parameters.defaultLibrary
    } else digitLibrary
  }

  // search on all positions for potential hits (don't count the "empty"/"zero" fields
  // TODO remove, see cellNumbers
  def detectedNumbers(hitCounts: HitCounts, cap: Int): Iterable[SCount] = {
    hitCounts.map { case hitcount => {
      val filtered = hitcount.drop(1).filter(_ >= cap)
      if (filtered.isEmpty) None else Some(filtered.max)
    }
    }.flatten
  }

  def computeSolution(digitLibrary: DigitLibrary): Future[(Option[SudokuDigitSolution], Option[Cells])] =
    Future {
      val someDigitSolution =
        (if (detectedNumbers(hCounts, cap).size > minHits)
          solve(mkSudokuMatrix(hCounts, cap))
        else Some(mkIntermediateSudokuMatrix(hCounts)))
      val someCells = someDigitSolution.map(s => toSolutionCells(digitLibrary, s))
      (someDigitSolution, someCells)
    }

  private def solve(solutionCandidate: SudokuDigitSolution) = BruteForceSolver.solve(solutionCandidate)

  def withCap(cap: Int)(v: Int) = v == cap

  def mkSudokuMatrix(hitCounts: HitCounts, cap: Int): SudokuDigitSolution = mkVM(hitCounts)(withCap(cap)(_))

  def mkIntermediateSudokuMatrix(hitCounts: HitCounts): SudokuDigitSolution = mkVM(hitCounts)(_ => true)

  // returns the sudoku matrix by analyzing the hitcounts array
  def mkVM(hitCounts: HitCounts)(p: Int => Boolean): SudokuDigitSolution = {
    val h =
      for (i <- cellRange) yield {
        ((for ((v, i) <- hitCounts(i).zipWithIndex if p(v)) yield i).headOption.getOrElse(0) + 48).toChar
      }
    h.toArray
  }


  /**
   * Performance:
   *
   * Benchmark                                          Mode   Samples         Mean   Mean error    Units
   * n.l.a.s.SudokuBenchmark.measureToSolutionCells     avgt        10        0.009        0.000    ms/op
   *
   * @return
   */
  def toSolutionCells(digitLibrary: DigitLibrary, digitSolution: SudokuDigitSolution): Cells = {
    val allCells: Cells =
      (for (pos <- cellRange) yield {
        val value = digitSolution(pos).asDigit

        val x: Option[SCell] =
          if (value != 0) {
            val someM = digitLibrary(value)._2
            (if (someM.isEmpty) {
              //              digitData(value) = mkFallback(value, digitData)
              digitLibrary(value)._2
            } else someM)
              .map(m => SCell(value, 0, new Rect))
          } else None
        x
      }).flatten.toArray

    allCells
  }


  /**
   * paints green borders around the cells
   * @param canvas
   * @param rects
   * @param someSolution
   * @param hitCounts
   * @return
   */
  def paintCorners(canvas: Mat,
                   rects: Seq[Rect],
                   someSolution: Option[Cells],
                   hitCounts: HitCounts
                    ): Future[Mat] = {


    // TODO update colors
    def color(hitCounts: HitCounts, i: Int, cap: Int): Scalar = {
      val vals = hitCounts(i)
      val n = vals.max.toDouble
      val s = new Scalar(0, n * 256 / cap, 256 - n * 256 / cap)
      s
    }

    def isValid(solution: Cells): Boolean = {
      solution.foldLeft(0)((acc, s) => acc + s.value) == 405
    }


    Future {
      for (solution <- someSolution if isValid(solution)) {
        traverseWithIndex(rects)((cell, i) =>
          paintRect(canvas, rects(i), color(hitCounts, i, cap), 1)
        )
      }

      canvas
    }
  }

}

object CornerDetector {

  val EmptyCorners = new MatOfPoint2f
}

case class CornerDetector(dilated: Mat) {

  val corners: MatOfPoint2f = detectSudokuCorners(dilated)

  val foundCorners: Boolean = {
    !corners.empty
  }

}

case class Warper(frame: Mat, destCorners: MatOfPoint2f) {

  val sudokuCanvas: SudokuCanvas = warp(frame, destCorners, mkCorners(frame.size))

}

case class CellDetector(sudokuCanvas: SudokuCanvas) {

  val cellSize = mkCellSize(sudokuCanvas.size)
  val cellRects: Seq[Rect] = cellRange.map(mkRect(_, cellSize))
  val futureSCells: Seq[Future[SCell]] = cellRects.map(detectCell(sudokuCanvas, _))

  // 81 possibly detected cells, most of them probably filled with 0's
  val futureDetectedCells: Future[Seq[SCell]] = Future.fold(futureSCells)(Seq[SCell]())((cells, c) => cells ++ Seq(c))
}


/**
 *
 * @param nr number of the frame
 * @param frame the frame information itself
 */
case class SCandidate(nr: Int, frame: Mat) extends CanLog {

  val start = System.nanoTime()

  private val imageIoChain: ImageIOChain = ImageIOChain(frame)

  val cornerDetector: CornerDetector = CornerDetector(imageIoChain.dilated)

  lazy val warper = Warper(frame, cornerDetector.corners)

  lazy val cellDetector: CellDetector = CellDetector(warper.sudokuCanvas)


  def mergeHits(currentHitCounts: HitCounds, detections: Seq[Int]): HitCounds = {
    (for ((value, index) <- detections.zipWithIndex) yield {
      val frequencies: Map[Int, Int] = currentHitCounts(index)
      index -> (frequencies + (value -> (frequencies(value) + 1)))
    }).toMap
  }
  /**
   * This function uses an input image and a detection method to calculate the sudoku.
   */
  def calc(currentState: SudokuState,
           lastDigitLibrary: DigitLibrary,
           lastHits : HitCounds): Future[(SudokuResult, DigitLibrary, HitCounds)] = {

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline
    if (cornerDetector.foundCorners) {
      for {
        detectedCells <- cellDetector.futureDetectedCells
        //
        currentDigitLibrary0 = lastDigitLibrary ++ currentState.merge(warper.sudokuCanvas, lastDigitLibrary, detectedCells)
        currentDigitLibrary = currentState.updateLibrary(currentDigitLibrary0, detectedCells)
        currentHits = mergeHits(lastHits, detectedCells.map(_.value))

        (someDigitSolution, someSolutionCells) <- currentState.computeSolution(currentDigitLibrary)

        withSolution <- paintSolution(cellDetector.sudokuCanvas,
          detectedCells.map(_.value),
          someSolutionCells,
          currentDigitLibrary,
          cellDetector.cellRects)
        annotatedSolution <- currentState.paintCorners(withSolution, cellDetector.cellRects, someSolutionCells, currentState.hCounts)

        unwarped = warp(annotatedSolution, mkCorners(frame.size), cornerDetector.corners)
        //blurry <- blur(frame)
        //solutionMat <- copySrcToDestWithMask(unwarped, imageIoChain.working, unwarped) // copy solution mat to input mat
        solutionMat <- copySrcToDestWithMask(unwarped, frame, unwarped) // copy solution mat to input mat
      } yield {
        if (someSolutionCells.isDefined) {
          (SSuccess(nr,
            frame,
            start,
            imageIoChain,
            warper.sudokuCanvas,
            cornerDetector.foundCorners,
            detectedCells.toArray,
            someDigitSolution.get,
            solutionMat,
            cornerDetector.corners.toList.toList), currentDigitLibrary, currentHits)
        } else {
          (SCorners(nr,
            frame,
            start,
            imageIoChain,
            warper.sudokuCanvas,
            detectedCells.toArray,
            cornerDetector.corners.toList.toList), currentDigitLibrary,currentHits)
        }
      }
    } else {
      Future.successful((SFailure(nr, frame, start, imageIoChain), lastDigitLibrary, lastHits))
    }

  }


  /**
   * paints the solution to the canvas.
   *
   * returns the modified canvas with the solution painted upon.
   *
   * detectedCells contains values from 0 to 9, with 0 being the cells which are 'empty' and thus have to be filled up
   * with numbers.
   *
   * uses digitData as lookup table to paint onto the canvas, thus modifying the canvas.
   */
  private def paintSolution(canvas: Mat,
                            detectedCells: Seq[Int],
                            someSolution: Option[Cells],
                            digitLibrary: DigitLibrary,
                            rects: Seq[Rect]): Future[Mat] = {

    Future {
      for (solution <- someSolution) {
        val values = solution.map(_.value)
        for ((s, r) <- values zip rects if values.sum == 405) {
          copyTo(digitLibrary(s)._2.getOrElse(mkFallback(s, digitLibrary).get), canvas, r)
        }
      }
      canvas
    }
  }

  /**
   * provides a fallback if there is no digit detected for this number.
   *
   * the size and type of the mat is calculated by looking at the other elements of the digit
   * library. if none found there, just returns null
   *
   * @param number
   * @return
   */
  private def mkFallback(number: Int, digitLibrary: DigitLibrary): Option[Mat] = {
    /**
     * returns size and type of Mat's contained int he digitLibrary
     * @return
     */
    def determineMatParams(): Option[(Size, Int)] = {
      digitLibrary.values.map(_._2).flatten.headOption.map {
        case m => (m.size, m.`type`)
      }
    }

    for ((size, matType) <- determineMatParams()) yield {
      val mat = new Mat(size.height.toInt, size.width.toInt, matType).setTo(new Scalar(255, 255, 255))
      Core.putText(mat, number.toString, new Point(size.width * 0.3, size.height * 0.9), Core.FONT_HERSHEY_TRIPLEX, 2, new Scalar(0, 0, 0))
      mat
    }
  }


}



