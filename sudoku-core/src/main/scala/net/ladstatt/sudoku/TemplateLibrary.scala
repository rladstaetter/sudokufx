package net.ladstatt.sudoku

import java.io.InputStream

import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV
import org.opencv.core.{Mat, Size}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source

/**
 * Created by lad on 15.03.15.
 */
object TemplateLibrary extends CanLog {
  private val (templateWidth, templateHeight) = (25.0, 50.0)
  val templateSize = new Size(templateWidth, templateHeight)
  var getResourceAsStream: String => InputStream = getClass.getResourceAsStream
  var templateResource: String = "/net/ladstatt/apps/sudokufx/templates.csv"

  lazy val asSeq: Seq[Mat] = logWithTimer("Initialize templates", {
    val digits: Seq[Array[Int]] =
      Source.fromInputStream(getResourceAsStream(templateResource)).getLines().map(l => l.split(",").map(e => if (e == "0") 0 else 255)).toSeq

    digits.map(OpenCV.toMat(_, templateSize))
  })


  /**
   * given a template library, match the given contour to find the best match. this function takes around 1 ms
   *
   * @return
   */
  def detectNumber(candidate: Mat): Future[(SNum, SHitQuality)] = {
    val resizedCandidate = OpenCV.resize(candidate, TemplateLibrary.templateSize) // since templates are 25 x 50
    val matchHaystack: (SIndex, VideoInput) => Future[(SIndex, SHitQuality)] = OpenCV.matchTemplate(resizedCandidate, _: Int, _: Mat)

    val result: Future[(SIndex, SHitQuality)] =
      for {s <- Future.sequence(for {(needle, number) <- TemplateLibrary.asSeq.zipWithIndex} yield
      for {(number, quality) <- matchHaystack(number + 1, needle)} yield (number, quality))
      } yield s.toSeq.sortWith((a, b) => a._2 < b._2).head


    result
  }


}
