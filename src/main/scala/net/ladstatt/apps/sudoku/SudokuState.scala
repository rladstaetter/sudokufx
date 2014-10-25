package net.ladstatt.apps.sudoku

import java.util.concurrent.TimeUnit

import net.ladstatt.apps.sudoku.Parameters._
import net.ladstatt.apps.sudoku.SudokuAlgos._
import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._
import org.opencv.imgproc.Imgproc

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
 * the result for one frame. a frame is a image from the image stream
 */

case class ImageIOChain(working: Mat,
                        grayed: Mat,
                        blurred: Mat,
                        thresholded: Mat,
                        inverted: Mat,
                        dilated: Mat,
                        eroded: Mat)


case class FrameSuccess(solution: Mat,
                        detectedCells: Cells,
                        digitSolution: Option[SudokuDigitSolution],
                        solutionCells: Option[Cells])

/**
 *
 * @param nr number of the frame
 * @param frame the frame information itself
 * @param cap how often should a digit be detected before it is considered "stable enough"
 * @param minHits how many digits have to be detected before a solution attempt is executed
 * @param hitCounts the individual detection numbers
 * @param digitQuality indicates the best hit for each number
 * @param digitData saves the picture information for the best hit
 * @param someResult either None or the solution for this frame
 */
case class SudokuState(nr: Int,
                       frame: Mat,
                       cap: Int = 8,
                       minHits: Int = 20,
                       // for each of the 81 sudoku cells, there exists a list which depicts how often a certain number
                       // was found in the sudoku, where the index in the list is the number (from 0 to 9, with 0 being
                       // the "empty" cell)
                       hitCounts: Array[HitCount] = Array.fill(positions.size)(Array.fill[SCount](digitRange.size)(0)),
                       digitQuality: Array[Double] = Array.fill(digitRange.size)(Double.MaxValue),
                       digitData: Array[Option[Mat]] = Array.fill(digitRange.size)(None),
                       someResult: Option[FrameSuccess] = None,
                       blockSizes: Array[Size] = Array.fill(cellCount)(new Size)) extends CanLog {

  val start = System.nanoTime()

  val corners = mkCorners(frame.size)

  val imageIoChain: ImageIOChain =
    Await.result(for {
      imgIo <- imageIOChain(frame)
    } yield imgIo, Duration(1400, TimeUnit.MILLISECONDS))

  val detectedCorners: MatOfPoint2f =
    Await.result(for {
      corners <- detectSudokuCorners(imageIoChain.dilated) // if (!detectedCorners.empty)
    } yield corners, Duration(1400, TimeUnit.MILLISECONDS))

  lazy val futureWarped = warp(frame, detectedCorners, corners)


  /**
   * This function uses an input image and a detection method to calculate the sudoku.
   *
   * @return
   */
  def calc(): Future[SudokuState] = {

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline
    if (!detectedCorners.empty) {
      for {colorWarped <- futureWarped
           detectedCells <- Future.sequence(detectCells(colorWarped))
           s0 <- updateLibrary(detectedCells)
           _ <- countHits(detectedCells)
           _ <- resetIfInvalidCellsDetected(detectedCells)
           someDigitSolution <- computeSolution()
           someSolutionCells <- Future.successful(for (solution <- someDigitSolution) yield toSolutionCells(solution))
           annotatedSolution <- paintSolution(colorWarped, detectedCells, someSolutionCells)
           unwarped <- warp(annotatedSolution, corners, detectedCorners)
           solution <- copySrcToDestWithMask(unwarped, imageIoChain.working, unwarped) // copy solution mat to input mat
      } yield copy(someResult = Some(FrameSuccess(solution, detectedCells.toArray, someDigitSolution, someSolutionCells)))
    } else {
      Future.successful(copy())
    }

  }


  def detectSudokuCorners(preprocessed: Mat): Future[MatOfPoint2f] = {
    execFuture {
      val epsilon = 0.02

      extractCurveWithMaxArea(coreFindContours(preprocessed)) match {
        case None => {
          logWarn("Could not detect any curve ... ")
          new MatOfPoint2f()
        }
        case Some((maxArea, c)) => {
          val expectedMaxArea = Imgproc.contourArea(corners) / 30
          val approxCurve = mkApproximation(new MatOfPoint2f(c.toList: _*), epsilon)
          if (maxArea > expectedMaxArea) {
            if (has4Sides(approxCurve)) {
              val corners = mkSortedCorners(approxCurve)
              if (isSomewhatSquare(corners.toList)) {
                corners
              } else {
                logWarn(s"Detected ${approxCurve.size} shape, but it doesn't look like a sudoku!")
                new MatOfPoint2f()
              }
            } else {
              logWarn(s"Detected only ${approxCurve.size} shape, but need 1x4!")
              new MatOfPoint2f()
            }
          } else {
            logWarn(s"The detected area of interest was too small ($maxArea < $expectedMaxArea).")
            new MatOfPoint2f()
          }
        }
      }
    }

  }

  def computeSolution(): Future[Option[SudokuDigitSolution]] =
    Future {
      if (detectedNumbers.size > minHits) solve(mkSudokuMatrix) else Some(mkIntermediateSudokuMatrix)
    }


  /**
   * paints the solution to the canvas.
   *
   * returns the modified canvas with the solution painted upon.
   */
  private def paintSolution(canvas: Mat, detectedCells: Seq[SCell], someSolution: Option[Cells]): Future[Mat] =
    Future {
      for (solution <- someSolution) {
        if (isValid(solution)) {
          // only copy cells which are not already known
          for ((cell, i) <- solution.zipWithIndex if (detectedCells(i).value == 0)) {
            copyTo(cell.data, canvas, mkRect(i, cell.data.size))
          }
        } else {
          detectedCells.zipWithIndex.map { case (cell, i) => {
            paintRect(canvas, mkRect(i, cell.data.size), color(i), 3)
          }
          }
        }
      }
      canvas
    }


  // TODO update colors
  private def color(i: Int): Scalar = {
    val vals = hitCounts(i)
    val n = vals.max.toDouble
    val s = new Scalar(0, n * 256 / cap, 256 - n * 256 / cap)
    s
  }


  def isValid(solution: Cells): Boolean = {
    solution.foldLeft(0)((acc, s) => acc + s.value) == 405
  }

  private def solve(solutionCandidate: SudokuDigitSolution) = BruteForceSolver.solve(solutionCandidate)

  val qualityFilter: PartialFunction[SCell, Boolean] = {
    case c => (c.value != 0) && (c.quality < digitQuality(c.value)) // lower means "better"
  }

  def updateLibrary(detectedCells: Traversable[SCell]): Future[SudokuState] = execFuture {
    val hits = detectedCells.filter(qualityFilter)
    hits.foreach(c => {
      digitData(c.value) = Some(c.data)
      digitQuality(c.value) = c.quality
    })
    this
  }


  private def sectorWellFormed(index: Pos, value: Int): Boolean = {
    val rowSector = sectors(row(index) / 3)
    val colSector = sectors(col(index) / 3)
    val sectorVals =
      for {r <- rowSector if (r != row(index))
           c <- colSector if (c != col(index))
           (count, num) <- hitCounts(index).zipWithIndex if (count == cap)} yield num
    !sectorVals.contains(value)
  }

  private def rowColWellFormed(i: Int, value: Int): Boolean = {
    val colVals =
      for {c <- range if (c != col(i) &&
        hitCounts(i).contains(value) &&
        hitCounts(i)(value) == cap)} yield value

    val rowVals =
      for {r <- range if (r != row(i) &&
        hitCounts(i).contains(value) &&
        hitCounts(i)(value) == cap)} yield value

    colVals.isEmpty && rowVals.isEmpty
  }

  private def posWellFormed(i: Pos, value: Int): Boolean = {
    rowColWellFormed(i, value) && sectorWellFormed(i, value)
  }

  /**
   * updates the hit database.
   *
   * @param cells
   */
  def countHits(cells: Seq[SCell]): Future[Unit] = Future {

    def updateHitCounts(i: Pos, value: Int): Unit = {
      val hitCountAtPos = hitCounts(i)
      if (hitCountAtPos.max < cap) {
        hitCountAtPos(value) = (1 + hitCountAtPos(value))
      }
    }

    cells.zipWithIndex.map { case (c, i) if ((c.value == 0) || posWellFormed(i, c.value)) => updateHitCounts(i, c.value)}
    ()

  }

  def resetIfInvalidCellsDetected(cells: Seq[SCell]): Future[Unit] = Future {
    if (cells.filter(c => c.value != 0).zipWithIndex.forall { case (c, i) => posWellFormed(i, c.value)}) {
      hitCounts.transform(_ => Array.fill[SCount](Parameters.digitRange.size)(0))
      digitData.transform(_ => None)
      digitQuality.transform(_ => Double.MaxValue)
      ()
    }
  }


  // search on all positions for potential hits (don't count the "empty"/"zero" fields
  def detectedNumbers: Iterable[SCount] = {
    (for {
      frequency <- hitCounts
    } yield {
      val filtered = frequency.drop(1).filter(_ >= cap)
      if (filtered.isEmpty) None else Some(filtered.max)
    }).flatten
  }

  def withCap(v: Int) = v == cap

  def mkSudokuMatrix: SudokuDigitSolution = mkVM(withCap(_))

  def mkIntermediateSudokuMatrix: SudokuDigitSolution = mkVM(_ => true)

  // returns the sudoku matrix by analyzing the hitcounts array
  def mkVM(p: Int => Boolean): SudokuDigitSolution = {
    val h =
      for (i <- positions) yield {
        ((for ((v, i) <- hitCounts(i).zipWithIndex if p(v)) yield i).headOption.getOrElse(0) + 48).toChar
      }
    (for (line <- h.sliding(9, 9)) yield line.toArray).toArray
  }


  /**
   * Performance:
   *
   * Benchmark                                          Mode   Samples         Mean   Mean error    Units
   * n.l.a.s.SudokuBenchmark.measureToSolutionCells     avgt        10        0.009        0.000    ms/op
   *
   * @return
   */
  private def toSolutionCells(solution: SudokuDigitSolution): Cells = {
    val digitSolution = solution.flatten
    val allCells: Cells =
      (for (pos <- positions) yield {
        val value = digitSolution(pos).asDigit

        val x: Option[SCell] =
          if (value != 0) {
            val someM = digitData(value)
            (if (someM.isEmpty) {
              digitData(value) = mkFallback(value, digitData)
              digitData(value)
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
      digitData.flatten.headOption.map { case m => (m.size, m.`type`)}
    }

    for ((size, matType) <- determineMatParams(digitData)) yield {
      val mat = new Mat(size.height.toInt, size.width.toInt, matType).setTo(new Scalar(255, 255, 255))
      Core.putText(mat, number.toString, new Point(size.width * 0.3, size.height * 0.9), Core.FONT_HERSHEY_TRIPLEX, 2, new Scalar(0, 0, 0))
      mat
    }
  }


}