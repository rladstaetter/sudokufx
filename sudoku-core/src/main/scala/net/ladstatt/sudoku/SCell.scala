package net.ladstatt.sudoku

import java.nio.file.{Path, Paths}

import net.ladstatt.sudoku.JavaCV.findContours
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{Mat, Point, Range, Rect}


/**
 * Represents a cell of the sudoku, that is one single number (a typical sudoku consists of 81)
 *
 * Every cell has its value, a region of interest and some other attributes.
 *
 * @param pos        cell position
 * @param cellMat    video input for this specific area
 * @param roi        region of interest; coordinates for this cell in main canvas
 * @param hitHistory what the image recognition thought in the past of this particular cell. a distribution of hits,
 *                   for example following entry: Map(1 -> 10) means that 10 times a '1' was detected for this
 *                   particular cell
 */
case class SCell(id: String
                 , pos: Int
                 , cellMat: Mat
                 , roi: Rect
                 , hitHistory: Map[Int, Int]) {
  private val targetPath: Path = Paths.get("target/sessions").resolve(System.currentTimeMillis().toString)

  JavaCV.writeMat("0-cellmat", id, pos, targetPath, cellMat)

  val cell = doitWith("1-grayed", JavaCV.toGray, targetPath)(cellMat)


  // only search for contours in a subrange of the original cell to get rid of possible border lines
  val (width, height) = (cell.size.width, cell.size.height)
  val cellData = new Mat(cell, new Range((height * 0.1).toInt, (height * 0.9).toInt), new Range((width * 0.1).toInt, (width * 0.9).toInt))
  val cellArea = cellData.size().area
  val (minArea, maxArea) = (0.05 * cellArea, 0.8 * cellArea)
  private val cellCenter = new Point(cellData.size.width / 2, cellData.size.height / 2)

  val blurred = doitWith("2-blurred", JavaCV.gaussianblur, targetPath)(cellData)
  val equalized = doitWith("3-equalized", JavaCV.equalizeHist, targetPath)(blurred)
  val thresholed = doitWith("4-threshold", JavaCV.threshold, targetPath)(equalized)
  val bitNotted = doitWith("5-bitnotted", JavaCV.bitwiseNot, targetPath)(thresholed)
  val flooded = doitWith("6-flooded", JavaCV.floodFillCorners, targetPath)(bitNotted)

  def doitWith(name: String, op: Mat => Mat, parentFolder: Path)(mat: Mat): Mat = {
    val res: Mat = op(mat)
    JavaCV.writeMat(name, id, pos, parentFolder, res)
    res
  }

  val optNumberCellMat: Option[Mat] = optNumberMat(flooded)

  val (detectedValue, quality) =
    optNumberCellMat.map {
      m => TemplateLibrary.detectNumber(id, pos, targetPath, m)
    }.getOrElse((0, 0.0))

  /** adds current hit to history, ignoring 0 */
  val updatedHits: Map[Int, Int] = {
    if (detectedValue == 0) {
      hitHistory
    } else {
      hitHistory + (detectedValue -> (hitHistory.getOrElse(detectedValue, 0) + 1))
    }
  }

  /** returns Some(value) if a certain threshold of hits are made for this cell, else None */
  val optValue: Option[Int] =
    updatedHits.toSeq.find {
      case (_, numberOfHits) => numberOfHits > Sudoku.minNrOfValueHits
    }.map(_._1)


  /**
   * detect false positives in a cell.
   *
   * heuristics applied:
   *
   * - throw away everything which isn't big or small enough enough to be considered a detected number.
   * - throw away all contours which don't contain the center of the given cell mat
   *
   * @param floodedRawCell cell mat where several image operations where applied in order to detect a number
   * @return
   */
  def optNumberMat(floodedRawCell: Mat): Option[Mat] = {

    // get all countours
    val allContours = findContours(floodedRawCell, opencv_imgproc.RETR_TREE, opencv_imgproc.CHAIN_APPROX_SIMPLE).get()

    // filter best contours and apply resulting rect to cellmat
    findBoundingBox(allContours).map(floodedRawCell.apply)

  }

  private def findBoundingBox(detectedContours: Array[Mat]): Option[Rect] = {
    detectedContours.foldLeft[Option[(Double, Mat)]](None) {
      case (acc, contourMat) =>
        val boundingRect = opencv_imgproc.boundingRect(contourMat)
        val boundingContourArea = boundingRect.area
        //        println("boundingContourArea: " + boundingContourArea)
        //        println("minArea: " + minArea)
        //        println("maxArea: " + maxArea)
        val contourAreaBiggerThanMinArea = boundingContourArea > minArea
        val contourAreaSmallerThanMaxArea = boundingContourArea < maxArea
        val contourContainsCenter = boundingRect.contains(cellCenter)
        if (contourAreaBiggerThanMinArea &&
          contourAreaSmallerThanMaxArea // &&          contourContainsCenter
        ) {
          val area = opencv_imgproc.contourArea(contourMat)
          acc match {
            case None => Some((area, contourMat))
            case Some((a, _)) =>
              if (area > a) {
                Some((area, contourMat))
              } else {
                acc
              }
          }
        } else acc
    } map {
      case (_, bestContour) => opencv_imgproc.boundingRect(bestContour)
    }
  }

}
