package net.ladstatt.apps.sudoku

import java.io.InputStream

import net.ladstatt.opencv.OpenCV
import org.opencv.core.{Mat, Size}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source

object TemplateLibrary {
  val (templateWidth, templateHeight) = (25.0, 50.0)
  val templateSize = new Size(templateWidth, templateHeight)
  lazy val sudokuTemplateSize = new Size(templateWidth * Parameters.ssize, templateHeight * Parameters.ssize)

  lazy val templateCorners = OpenCV.mkCorners(sudokuTemplateSize)
  var getResourceAsStream: String => InputStream = getClass.getResourceAsStream
  var templateResource: String = "/net/ladstatt/apps/sudokufx/templates.csv"

  lazy val asSeq: Seq[Mat] = {
    val digits: Seq[Array[Int]] =
      Source.fromInputStream(getResourceAsStream(templateResource)).getLines().map(l => l.split(",").map(e => if (e == "0") 0 else 255)).toSeq

    digits.map(OpenCV.toMat(_, templateSize))
  }


  /**
   * given a template library, match the given contour to find the best match. this function takes around 1 ms
   *
   * @return
   */
  def detectNumber(candidate: Mat): Future[(SNum, SHitQuality)] = {
    val resizedCandidate = OpenCV.resize(candidate, TemplateLibrary.templateSize) // since templates are 25 x 50
    val matchHaystack: (SIndex, SudokuCanvas) => Future[(SIndex, SHitQuality)] = OpenCV.matchTemplate(resizedCandidate, _: Int, _: Mat)


    val result: Future[(SIndex, SHitQuality)] =
      for {s <- Future.sequence(for {(needle, number) <- TemplateLibrary.asSeq.zipWithIndex} yield
      for {(number, quality) <- matchHaystack(number + 1, needle)} yield (number, quality))
      } yield s.toSeq.sortWith((a, b) => a._2 < b._2).head


    result
  }


}


object Parameters {


  // number of different values a cell can have before the cell is label 'ambiguous'
  val ambiguitiesCount = 5

  // how many cells are allowed to have ambiguous information before number detection process is restarted
  val ambiCount = 5

  // numbers won't get any larger in the status matrix than this number
  val topCap = 5

  // least number of matches necessary to identify one number
  // if you have a good camera, take 1 to get fast response
  val cap = 3

  assert(topCap - cap > 0)

  val minHits = 22

  val ssize = 9
  val cellCount = ssize * ssize

  val range = 0 until ssize
  val digitRange = 0 to ssize

  val cellRange: Range = 0 until cellCount

  val colorRange = 0 to 256 by 16
  private val leftRange: Seq[Int] = Seq(0, 1, 2)
  private val middleRange: Seq[Int] = Seq(3, 4, 5)
  private val rightRange: Seq[Int] = Seq(6, 7, 8)
  val sectors: Seq[Seq[Int]] = Seq(leftRange, middleRange, rightRange)


  val defaultDigitLibrary: DigitLibrary = Map().withDefaultValue((Double.MaxValue, None))
  val defaultHitCounters: HitCounters = Map().withDefaultValue(Map[Int, Int]().withDefaultValue(0))


  def row(i: SIndex): Int = i / 9

  def col(i: SIndex): Int = i % 9
}
