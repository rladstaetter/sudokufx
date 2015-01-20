package net.ladstatt.apps.sudoku

import net.ladstatt.apps.sudoku.Parameters._
import net.ladstatt.apps.sudoku.SudokuAlgos._
import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

sealed trait SudokuResult {
  /*def nr: Int

  def frame: Mat

  def start: Long
  */
}

case class SCorners(nr: Int,
                    frame: Mat,
                    start: Long,
                    imageIOChain: ImageIOChain,
                    detectedCells: Cells,
                    sudokuCorners: List[Point]) extends SudokuResult

case class SSuccess(nr: Int,
                    frame: Mat,
                    start: Long,
                    imageIOChain: ImageIOChain,
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


  def duplicate(origHCounts: HitCounts): HitCounts = {
    for (cnts <- origHCounts) yield {
      for (x <- cnts) yield x
    }
  }

  def duplicate(origDigitQuality: Array[Double]): Array[Double] = origDigitQuality.clone()

  def duplicate(origDigitData: Array[Option[Mat]]): Array[Option[Mat]] = origDigitData.clone()

  def apply(orig: SCandidate): SCandidate = {
    SCandidate(
      nr = orig.nr,
      frame = copyMat(orig.frame))
  }

}

object SudokuState {

  def duplicate(origHCounts: HitCounts): HitCounts = {
    for (cnts <- origHCounts) yield {
      for (x <- cnts) yield x
    }
  }

  def duplicate(origDigitQuality: Array[Double]): Array[Double] = origDigitQuality.clone()

  def duplicate(origDigitData: Array[Option[Mat]]): Array[Option[Mat]] = origDigitData.clone()

  def apply(orig: SudokuState): SudokuState = {
    SudokuState(hCounts = duplicate(orig.hCounts),
      digitQuality = duplicate(orig.digitQuality),
      digitData = duplicate(orig.digitData),
      cap = orig.cap,
      minHits = orig.minHits,
      cells = orig.cells.map(_.duplicate())
    )
  }
}

/**
 * @param hCounts the individual detection numbers
 *                for each of the 81 sudoku cells, there exists a list which depicts how often a certain number
 *                was found in the sudoku, where the index in the list is the number (from 0 to 9, with 0 being
 *                the "empty" cell)
 * @param digitQuality indicates the best hit for each number
 * @param digitData saves the picture information for the best hit
 * @param cap how often should a digit be detected before it is considered "stable enough"
 * @param minHits how many digits have to be detected before a solution attempt is executed
 */
case class SudokuState(hCounts: HitCounts = Array.fill(cellRange.size)(Array.fill[SCount](digitRange.size)(0)),
                       digitQuality: Array[Double] = Array.fill(digitRange.size)(Double.MaxValue),
                       digitData: Array[Option[Mat]] = Array.fill(digitRange.size)(None),
                       cap: Int = 8,
                       minHits: Int = 20,
                       cells: Seq[SCell] = Seq(),
                       digitLibrary: Map[SNum, (SHitQuality, Option[Mat])] = Map().withDefaultValue((Double.MaxValue, None))) {

  def statsAsString(): String =
    s"""$digitQualityAsString
       |${hitCountsAsString(hCounts)}
       |""".stripMargin

  def digitQualityAsString: String =
    s"""Quality:
      |--------
      |${digitQuality.map(q => q).mkString("\n")}
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
   * The filter returns only cells which contain 'better match' cells.
   *
   * If there are cells containing '0' detected they are ignored.
   */
  val qualityFilter: PartialFunction[SCell, Boolean] = {
    case c => (c.value != 0) && (c.quality < digitQuality(c.value)) // lower means "better"
  }


  /**
   * From a given list of cells, choose the ones which are the best hits
   * @param detectedCells
   * @return
   */
  def updateLibrary(detectedCells: Seq[SCell]): Future[Unit] = execFuture {
    merge(detectedCells)
    val detectedValues: Seq[SIndex] = detectedCells.map(_.value)
    countHits(detectedValues)
    resetIfInvalidCellsDetected(detectedValues)
    ()
  }

  /**
   * Returns a Sudoku State which is populated with the values delivered by the detected Cells
   * if the quality of those cells is better than the already known cells.
   *
   * @param other
   * @return
   */
  def mergeN(other: SudokuState): SudokuState = time({
    val hits: Seq[SCell] = other.cells.filter(qualityFilter)
    val grouped: Map[Int, Seq[SCell]] = hits.groupBy(c => c.value)
    val optimal: Map[Int, SCell] = grouped.map { case (i, cls) => i -> cls.maxBy(c => c.quality)}
    // TODO add some sort of normalisation for each cell with such an effect that every cell has the same color 'tone'

    val x: Map[SNum, (SHitQuality, Some[Mat])] =
      (for ((i, c) <- optimal if digitQuality(c.value) > c.quality) yield (i, (c.quality, Some(copyMat(c.data))))).toMap

    copy(digitLibrary = digitLibrary ++ x,
      cells = other.cells.map(_.duplicate))
  }, t => logInfo(s"Merging took: ${t} micros"))


  def merge(detectedCells: Seq[SCell]): SudokuState = time({
    val hits: Seq[SCell] = detectedCells.filter(qualityFilter)
    val grouped: Map[Int, Seq[SCell]] = hits.groupBy(f => f.value)
    val optimal: Map[Int, SCell] = grouped.map { case (i, cells) => i -> cells.maxBy(c => c.quality)}
    // TODO add some sort of normalisation for each cell with such an effect that every cell has the same color 'tone'
    for (c <- optimal.values if (digitQuality(c.value) > c.quality)) {
      digitData(c.value) = Some(copyMat(c.data))
      digitQuality(c.value) = c.quality
    }
    SudokuState(this)
  }, t => logInfo(s"Merging took: ${t} micros"))


  def resetIfInvalidCellsDetected(cells: Seq[Int]): Unit = {
    if (!SCandidate.isValid(hCounts, cells, cap)) {

      hCounts.transform(_ => Array.fill[SCount](Parameters.digitRange.size)(0))
      digitData.transform(_ => None)
      digitQuality.transform(_ => Double.MaxValue)
      ()
      ???
    }
  }

  // search on all positions for potential hits (don't count the "empty"/"zero" fields
  // TODO remove, see cellNumbers
  def detectedNumbers(hitCounts: HitCounts, cap: Int): Iterable[SCount] = {
    (for {
      hitcount <- hitCounts
    } yield {
      val filtered = hitcount.drop(1).filter(_ >= cap)
      if (filtered.isEmpty) None else Some(filtered.max)
    }).flatten
  }

  def computeSolution(): Future[(Option[SudokuDigitSolution], Option[Cells])] =
    Future {
      val someDigitSolution =
        (if (detectedNumbers(hCounts, cap).size > minHits)
          solve(mkSudokuMatrix(hCounts, cap))
        else Some(mkIntermediateSudokuMatrix(hCounts)))
      val someCells = someDigitSolution.map(s => toSolutionCells(s))
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
  def toSolutionCells(digitSolution: SudokuDigitSolution): Cells = {
    val allCells: Cells =
      (for (pos <- cellRange) yield {
        val value = digitSolution(pos).asDigit

        val x: Option[SCell] =
          if (value != 0) {
            val someM = digitData(value)
            (if (someM.isEmpty) {
//              digitData(value) = mkFallback(value, digitData)
              digitData(value)
            } else someM)
              .map(SCell(value, 0, _))
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

case class CellDetector(frame: Mat, sudokuCorners: MatOfPoint2f) {

  val colorWarped = warp(frame, sudokuCorners, mkCorners(frame.size))
  val cellSize = mkCellSize(colorWarped.size)
  val cellRects: Seq[Rect] = cellRange.map(mkRect(_, cellSize))
  val cellMats: Seq[Mat] = cellRects.map(colorWarped.submat)
  val futureSCells: Seq[Future[SCell]] = cellMats.map(detectCell)

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

  lazy val cellDetector: CellDetector = CellDetector(frame, cornerDetector.corners)

  /**
   * This function uses an input image and a detection method to calculate the sudoku.
   */
  def calc(currentState: SudokuState): Future[SudokuResult] = {

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline
    if (cornerDetector.foundCorners) {
      for {
        detectedCells <- cellDetector.futureDetectedCells

        _ <- currentState.updateLibrary(detectedCells)

        (someDigitSolution, someSolutionCells) <- currentState.computeSolution()

        withSolution <- paintSolution(cellDetector.colorWarped,
          detectedCells.map(_.value),
          someSolutionCells,
          currentState.digitData,
          cellDetector.cellRects)
        annotatedSolution <- currentState.paintCorners(withSolution, cellDetector.cellRects, someSolutionCells, currentState.hCounts)

        unwarped = warp(annotatedSolution, mkCorners(frame.size), cornerDetector.corners)
        //blurry <- blur(frame)
        //solutionMat <- copySrcToDestWithMask(unwarped, imageIoChain.working, unwarped) // copy solution mat to input mat
        solutionMat <- copySrcToDestWithMask(unwarped, frame, unwarped) // copy solution mat to input mat
      } yield {
        if (someSolutionCells.isDefined) {
          SSuccess(nr,
            frame,
            start,
            imageIoChain,
            cornerDetector.foundCorners,
            detectedCells.toArray,
            someDigitSolution.get,
            solutionMat,
            cornerDetector.corners.toList.toList)
        } else {
          SCorners(nr,
            frame,
            start,
            imageIoChain,
            detectedCells.toArray,
            cornerDetector.corners.toList.toList)
        }
      }
    } else {
      Future.successful(SFailure(nr, frame, start, imageIoChain))
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
                            digitData: Array[Option[Mat]],
                            rects: Seq[Rect]): Future[Mat] = {

    Future {
      for (solution <- someSolution) {
        val values = solution.map(_.value)
        for ((s, r) <- values zip rects if values.sum == 405) {
          copyTo(digitData(s).getOrElse(mkFallback(s, digitData).get), canvas, r)
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
  private def mkFallback(number: Int, digitData: Array[Option[Mat]]): Option[Mat] = {
    /**
     * returns size and type of Mat's contained int he digitLibrary
     * @return
     */
    def determineMatParams(digitData: Array[Option[Mat]]): Option[(Size, Int)] = {
      digitData.flatten.headOption.map {
        case m => (m.size, m.`type`)
      }
    }

    for ((size, matType) <- determineMatParams(digitData)) yield {
      val mat = new Mat(size.height.toInt, size.width.toInt, matType).setTo(new Scalar(255, 255, 255))
      Core.putText(mat, number.toString, new Point(size.width * 0.3, size.height * 0.9), Core.FONT_HERSHEY_TRIPLEX, 2, new Scalar(0, 0, 0))
      mat
    }
  }


}



