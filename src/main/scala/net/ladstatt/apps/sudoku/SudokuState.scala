package net.ladstatt.apps.sudoku

import java.util.concurrent.TimeUnit

import net.ladstatt.apps.sudoku.Parameters._
import net.ladstatt.apps.sudoku.SudokuAlgos._
import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._

import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

sealed trait SudokuResult {
  def candidate: SCandidate
}

case class SSuccess(candidate: SCandidate,
                    detectedCells: Cells,
                    solution: SudokuDigitSolution,
                    solutionMat: Mat,
                    solutionCells: Cells,
                    sudokuCorners: List[Point]) extends SudokuResult {

  def solutionAsString: String = solution.sliding(9, 9).map(new String(_)).mkString("\n")
}

case class SFailure(candidate: SCandidate) extends SudokuResult

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

  def duplicate(frame: Mat): Mat = {
    val copiedFrame = new Mat
    frame.copyTo(copiedFrame)
    copiedFrame
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
      frame = duplicate(orig.frame),
      cap = orig.cap,
      minHits = orig.minHits,
      currentState = SudokuState(orig.currentState))
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
      digitData = duplicate(orig.digitData)
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
 */
case class SudokuState(hCounts: HitCounts = Array.fill(cellRange.size)(Array.fill[SCount](digitRange.size)(0)),
                       digitQuality: Array[Double] = Array.fill(digitRange.size)(Double.MaxValue),
                       digitData: Array[Option[Mat]] = Array.fill(digitRange.size)(None)) {

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
  def countHits(cells: Seq[Int], cap: Int): Unit = {

    def updateHitCounts(i: SIndex, value: Int): Unit = {
      val hitCountAtPos = hCounts(i)
      if (hitCountAtPos.max < cap) {
        hitCountAtPos(value) = (1 + hitCountAtPos(value))
      }
    }

    traverseWithIndex(cells)((c, i) => {
      if (SCandidate.posWellFormed(hCounts, i, c, cap)) {
        updateHitCounts(i, c)
      }
    })
  }

  val qualityFilter: PartialFunction[SCell, Boolean] = {
    case c => (c.value != 0) && (c.quality < digitQuality(c.value)) // lower means "better"
  }


  /**
   * From a given list of cells, choose the ones which are the best hits
   * @param detectedCells
   * @return
   */
  def updateLibrary(detectedCells: Seq[SCell], cap: Int): Future[Unit] = execFuture {
    merge(detectedCells)
    val detectedValues = detectedCells.map(_.value)
    countHits(detectedValues, cap)
    resetIfInvalidCellsDetected(detectedValues, cap)
    ()
  }

  def merge(detectedCells: Traversable[SCell]): Unit = {
    val hits: Traversable[SCell] = detectedCells.filter(qualityFilter)
    val grouped: Map[Int, Traversable[SCell]] = hits.groupBy(f => f.value)
    val optimal: Map[Int, SCell] = grouped.map { case (i, cells) => i -> cells.maxBy(c => c.quality)}
    optimal.values.foreach(c => {
      digitData(c.value) = Some(c.data)
      digitQuality(c.value) = c.quality
    })
    ()
  }


  def resetIfInvalidCellsDetected(cells: Seq[Int],
                                  cap: Int): Unit = {
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

  def computeSolution( cap: Int, minHits: Int): Future[Option[SudokuDigitSolution]] =
    Future {
      if (detectedNumbers(hCounts, cap).size > minHits)
        solve(mkSudokuMatrix(hCounts, cap))
      else Some(mkIntermediateSudokuMatrix(hCounts))
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

}


/**
 *
 * @param nr number of the frame
 * @param frame the frame information itself
 * @param cap how often should a digit be detected before it is considered "stable enough"
 * @param minHits how many digits have to be detected before a solution attempt is executed
 * @param currentState current state of affairs
 */
case class SCandidate(nr: Int,
                      frame: Mat,
                      cap: Int = 8,
                      minHits: Int = 20,
                      currentState: SudokuState) extends CanLog {


  val start = System.nanoTime()

  val frameCorners = mkCorners(frame.size)

  val imageIoChain: ImageIOChain =
    Await.result(for {
      imgIo <- imageIOChain(frame)
    } yield imgIo, Duration(1400, TimeUnit.MILLISECONDS))

  val sudokuCorners: MatOfPoint2f =
    Await.result(for {
      corners <- detectSudokuCorners(imageIoChain.dilated, frameCorners) // if (!detectedCorners.empty)
    } yield corners, Duration(1400, TimeUnit.MILLISECONDS))

  lazy val colorWarped = warp(frame, sudokuCorners, frameCorners)
  lazy val sudokuSize = mkCellSize(colorWarped.size)

  lazy val warpedToFit = OpenCV.resize(colorWarped, Parameters.sudokuTemplateSize)
  lazy val warpedToFitSize = mkCellSize(warpedToFit.size)

  lazy val rects: immutable.IndexedSeq[Rect] = cellRange.map(mkRect(_, sudokuSize))
  lazy val warpedToFitRects = cellRange.map(mkRect(_, warpedToFitSize))

  lazy val cellMats: IndexedSeq[Mat] = rects.map(colorWarped.submat(_))
  lazy val futureSCells: IndexedSeq[Future[SCell]] = cellMats.map(detectCell(_, currentState.digitQuality))


  /**
   * This function uses an input image and a detection method to calculate the sudoku.
   */
  def calc(): Future[SudokuResult] = {

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline
    if (foundCorners) {
      for {
        detectedCells <- Future.fold(futureSCells)(Seq[SCell]())((cells, c) => cells ++ Seq(c))

        _ <- currentState.updateLibrary(detectedCells, cap)

        someDigitSolution <- currentState.computeSolution(cap, minHits)

        someSolutionCells = someDigitSolution.map(s => toSolutionCells(s)) //for (solution <- someDigitSolution) yield toSolutionCells(solution))
        withSolution <- paintSolution(colorWarped, detectedCells.map(_.value), someSolutionCells)
        annotatedSolution <- paintCorners(withSolution, rects, someSolutionCells, currentState.hCounts, cap)

        unwarped = warp(annotatedSolution, frameCorners, sudokuCorners)
        //blurry <- blur(frame)
        //solutionMat <- copySrcToDestWithMask(unwarped, imageIoChain.working, unwarped) // copy solution mat to input mat
        solutionMat <- copySrcToDestWithMask(unwarped, frame, unwarped) // copy solution mat to input mat
      } yield {
        if (someDigitSolution.isDefined) {
          SSuccess(SCandidate(this),
            detectedCells.toArray,
            someDigitSolution.get,
            solutionMat,
            someSolutionCells.get,
            sudokuCorners.toList.toList)
        } else {
          SFailure(SCandidate(this))
        }
      }
    } else {
      Future.successful(SFailure(SCandidate(this)))
    }

  }


  def foundCorners: Boolean = {
    !sudokuCorners.empty
  }


  /**
   * paints the solution to the canvas.
   *
   * returns the modified canvas with the solution painted upon.
   */
  // potentially modifies canvas
  private def paintSolution(canvas: Mat,
                            detectedCells: Seq[Int],
                            someSolution: Option[Cells]): Future[Mat] =
    Future {
      for (solution <- someSolution if isValid(solution)) {
        time(traverseWithIndex(solution)((cell, i) => if (detectedCells(i) == 0) copyTo(cell.data, canvas, rects(i))), t => logTrace(s"copied in $t ms"))
      }
      canvas
    }

  /**
   * paints green borders around the cells
   * @param canvas
   * @param rects
   * @param someSolution
   * @param hitCounts
   * @return
   */
  private def paintCorners(canvas: Mat,
                           rects: IndexedSeq[Rect],
                           someSolution: Option[Cells],
                           hitCounts: HitCounts,
                           cap: Int): Future[Mat] =
    Future {
      for (solution <- someSolution if isValid(solution)) {
        traverseWithIndex(rects)((cell, i) =>
          paintRect(canvas, rects(i), color(hitCounts, i, cap), 1)
        )
      }

      canvas
    }


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


  /**
   * Performance:
   *
   * Benchmark                                          Mode   Samples         Mean   Mean error    Units
   * n.l.a.s.SudokuBenchmark.measureToSolutionCells     avgt        10        0.009        0.000    ms/op
   *
   * @return
   */
  private def toSolutionCells(digitSolution: SudokuDigitSolution): Cells = {
    val allCells: Cells =
      (for (pos <- cellRange) yield {
        val value = digitSolution(pos).asDigit

        val x: Option[SCell] =
          if (value != 0) {
            val someM = currentState.digitData(value)
            (if (someM.isEmpty) {
              currentState.digitData(value) = mkFallback(value, currentState.digitData)
              currentState.digitData(value)
            } else someM)
              .map(SCell(value, 0, _))
          } else None
        x
      }).flatten.toArray

    allCells
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



