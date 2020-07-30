package net.ladstatt.sudoku

import net.ladstatt.core.ClasspathAddress
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.opencv_core.{Mat, Size}

import scala.io.Source


case class MatCp(value: String) extends ClasspathAddress

/**
 * Created by lad on 15.03.15.
 */
object TemplateLibrary {

  private val templateWidth = 25
  private val templateHeight = 50

  val templateSize = new Size(templateWidth, templateHeight)
  val templateCanvasSize = new Size(templateWidth * Parameters.ssize, templateHeight * Parameters.ssize)

  def fromCsv(csv: String): Mat = ???

  def toCsv(mat: Mat) = ???

  lazy val modernTemplatesfromMatCp: Seq[Mat] = {
    val ts = for (i <- 1 to 9) yield {
      val threeChannelBmp = JavaCV.loadMat(getClass, MatCp(s"/net/ladstatt/sudoku/templates/modern/$i.bmp"))
      val res = new Mat
      opencv_imgproc.cvtColor(threeChannelBmp, res, opencv_imgproc.COLOR_BGR2GRAY)
      res
    }
    ts
  }
  lazy val classicTemplatesFromCsvCp: Seq[Mat] = {
    val digits: Seq[Array[Byte]] =
      Source.fromInputStream(getClass.getResourceAsStream("/net/ladstatt/sudoku/templates/classic/templates.csv")).getLines().map(l => l.split(",").map(e => if (e == "0") 0.toByte else 255.toByte)).toSeq
    val seq = digits.map(b => JavaCV.toMat(b, templateSize))
    seq
  }
  lazy val templatesFromCp: Seq[Mat] = modernTemplatesfromMatCp

  lazy val classicClasspathTemplatesZipped: Seq[Mat] = TemplateLibrary.templatesFromCp


  /**
   * given a template library, match the given contour to find the best match.
   *
   * @return returns the best match for given mat when compared to the template library
   */
  def detectNumber(library: Seq[Mat])
                  (candidate: Mat): (Int, Double) = {
    //writeMat(s"7_candidate", id, frameNr, pos, path, candidate)

    val (width, height) = (candidate.size.width, candidate.size.height)

    // JavaCV.writeMat(Sudoku.targetPath.resolve(s"SCell-candidate.png"), candidate)
    // idea: only resize candidate if it is bigger than template to save time
    // a list of numbers with their quality
    val hayStack: Seq[(Int, Double)] =
    for ((needle, idx) <- library.zipWithIndex) yield {
      val number = idx + 1
      if (needle.size().width > width || needle.size().height > height) {
        val resizedNeedle = JavaCV.resize(needle, new Size(width, height))
        // JavaCV.writeMat(Sudoku.targetPath.resolve(s"SCell-needle-resized-$number.png"), resizedNeedle)
        JavaCV.matchTemplate(candidate, resizedNeedle, number)
      } else {
        // JavaCV.writeMat(Sudoku.targetPath.resolve(s"SCell-needle.png-$number.png"), needle)
        JavaCV.matchTemplate(candidate, needle, number)
      }
    }
    /*
    for ((i, quality) <- hayStack) {
      println(s"Number $i : Quality " + f"$quality%1.0f")
    }
     */
    // only use options with a certain quality
    val (number: Int, quality: Double) = hayStack
      /*.filter {
      case (_, q) => q > Sudoku.minQuality
    }*/
      .sortWith {
      case (a, b) => a._2 < b._2
    }.headOption.getOrElse((0, 0.0))

    // println(s"took $number: " + f"$quality%1.0f")
    (number, quality)

  }

  val detector: Mat => (Int, Double) = detectNumber(classicClasspathTemplatesZipped)

  def detectNumber(candidate: Mat): (Int, Double) = detector(candidate)


}
