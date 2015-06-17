package net.ladstatt.apps.sudoku

import java.io.File

import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV
import net.ladstatt.opencv.OpenCV._
import org.opencv.core._

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

/**
 *
 * @param nr number of the frame
 * @param frame the frame information itself
 */
case class SCandidate(nr: Int, frame: Mat) extends CanLog {


  def persist(file: File): Try[File] = {
    Try(file)
//    OpenCV.persist(frame, file)
  }

  val start = System.nanoTime()

  private val imageIoChain: ImageIOChain = ImageIOChain(frame)

  val cornerDetector: CornerDetector = CornerDetector(imageIoChain.dilated)

  lazy val warper = Warper(frame, cornerDetector.corners)

  lazy val cellDetector: CellDetector = CellDetector(warper.sudokuCanvas)


  def mergeHits(currentHitCounts: HitCounters, detections: Seq[Int], cap: Int): HitCounters = {
    val hits =
      (for ((value, index) <- detections.zipWithIndex) yield {
        val frequencies: Map[Int, Int] = currentHitCounts(index)
        index -> (frequencies + (value -> (frequencies(value) + 1)))
      }).toMap

    /*
if (!NewCandidate.isValid(hits, detections, cap)) {
logError("An invalid hitcount distribution found, resetting ...")
Parameters.defaultHitCounts
} else {
resetHitsIfThereAreTooMuchAmbiguities(hits)
}                                            */
    resetHitsIfThereAreTooMuchAmbiguities(hits)
  }

  // TODO add some sort of normalisation for each cell with such an effect that every cell has the same color 'tone'
  // TODO remove sudokuCanvas from signature: just save roi's and calculate Mats on demand
  def mergeDigitLibrary(sudokuCanvas: VideoInput,
                        digitLibrary: DigitLibrary,
                        detectedCells: Seq[SCell]): DigitLibrary = {

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
    val optimal: Map[Int, SCell] = grouped.map { case (i, cells) => i -> cells.maxBy(c => c.quality) }

    digitLibrary ++
      (for (c <- optimal.values if digitLibrary(c.value)._1 > c.quality) yield {
        val newData = Some(copyMat(sudokuCanvas.submat(c.roi)))
        c.value -> ((c.quality, newData))
      }).toMap
  }


  def resetHitsIfThereAreTooMuchAmbiguities(counters: HitCounters): HitCounters = {
    val cellAmbiguities = counters.values.map(m => m.size).count(_ > Parameters.ambiguitiesCount)
    if (cellAmbiguities > Parameters.ambiCount) {
      logError(s"Too many ambiguities ($cellAmbiguities), resetting .. ")
      Parameters.defaultHitCounters
    }
    else counters
  }

  /**
   * This function uses an input image and a detection method to calculate the sudoku.
   *
   * @param cap number of detections for a certain number until it is regarded as "stable enough"
   * @param minHits minimal number of numbers before a the solving is attempted
   * @param maxSolvingDuration number of milliseconds which the solver is given before he gives up
   */
  def calc(lastDigitLibrary: DigitLibrary,
           lastHits: HitCounters,
           cap: Int,
           minHits: Int,
           maxSolvingDuration: Long): Future[(SudokuResult, DigitLibrary, HitCounters)] = {

    // we have to walk two paths here: either we have detected something in the image
    // stream which resembles a sudoku, or we don't and we skip the rest of the processing
    // pipeline
    if (cornerDetector.foundCorners) {
      for {
        detectedCells <- cellDetector.futureDetectedCells
        mergedLibrary = mergeDigitLibrary(warper.sudokuCanvas, lastDigitLibrary, detectedCells)
        hitsToCompute = mergeHits(lastHits, detectedCells.map(_.value), cap)

        (someDigitSolution, someSolutionCells, currentHits, currentDigitLibrary) <- SudokuUtils.computeSolution(hitsToCompute, mergedLibrary, cap, minHits, maxSolvingDuration)

        withSolution <- paintSolution(cellDetector.sudokuCanvas,
          detectedCells.map(_.value),
          someSolutionCells,
          currentDigitLibrary,
          cellDetector.cellRects)
        annotatedSolution <- SudokuUtils.paintCorners(withSolution, cellDetector.cellRects, someSolutionCells, currentHits, cap)

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
            cornerDetector.corners.toList.toList), currentDigitLibrary, currentHits)
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
