package net.ladstatt.sudoku

import net.ladstatt.core.CanLog
import org.bytedeco.opencv.opencv_core.{Mat, Size}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source

/**
 * Created by lad on 15.03.15.
 */
object TemplateLibrary extends CanLog {

  private val templateWidth = 25
  private val templateHeight = 50

  val templateSize = new Size(templateWidth, templateHeight)
  val templateCanvasSize = new Size(templateWidth * Parameters.ssize, templateHeight * Parameters.ssize)
  //val warpedCorners: Mat = JavaCV.mkCorners(TemplateLibrary.templateCanvasSize.width, TemplateLibrary.templateCanvasSize.height)


  // todo: Map[Int,Mat]
  lazy val classicTemplatesFromClasspath: Seq[Mat] = {
    val digits: Seq[Array[Byte]] =
      Source.fromInputStream(getClass.getResourceAsStream("/net/ladstatt/sudoku/templates/classic/templates.csv")).getLines().map(l => l.split(",").map(e => if (e == "0") 0.toByte else 255.toByte)).toSeq
    digits.map(b => JavaCV.toMat(b, templateSize))
  }

  lazy val classicClasspathTemplatesZipped: Seq[(Mat, Int)] = TemplateLibrary.classicTemplatesFromClasspath.zipWithIndex


  /**
   * given a template library, match the given contour to find the best match.
   *
   * @return returns the best match for given mat when compared to the template library
   */
  def detectNumber(library: Seq[(Mat, Int)], templateSize: Size)(candidate: Mat): Future[(Int, Double)] = {
    val resizedCandidate = JavaCV.resize(candidate, templateSize)
    val matchHaystack: (Int, Mat) => Future[(Int, Double)] = JavaCV.matchTemplate(resizedCandidate, _: Int, _: Mat)

    val result: Future[(Int, Double)] =
      for {s <- Future.sequence(for {(needle, number) <- library} yield
        for {(number, quality) <- matchHaystack(number + 1, needle)} yield (number, quality))} yield s.sortWith((a, b) => a._2 < b._2).head

    result
  }

  val matToEventualTuple: Mat => Future[(Int, Double)] = detectNumber(classicClasspathTemplatesZipped, templateSize)

  def detectNumber(candidate: Mat): Future[(Int, Double)] = {
    matToEventualTuple(candidate)
  }

}
