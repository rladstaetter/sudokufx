package net.ladstatt.apps.sudoku

import net.ladstatt.opencv.OpenCV
import org.opencv.core._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._


object TemplateDetectionStrategy {


  /**
   * given a template library, match the given contour to find the best match. this function takes around 1 ms
   *
   * @return
   */
  def detectNumber(candidate: Mat): Future[(SNum, SHitQuality)] = {
    val resizedCandidate = OpenCV.resize(candidate, Parameters.templateSize) // since templates are 25 x 50
    val matchHaystack = OpenCV.matchTemplate(resizedCandidate, _: Mat, _: Int)

    val result =
      for {s <- Future.sequence(for {(number, needle) <- Parameters.templateLibrary} yield
        for {(number, quality) <- matchHaystack(needle, number)} yield (number, quality))
      } yield s.toSeq.sortWith((a, b) => a._2 < b._2).head

    result
  }



}










