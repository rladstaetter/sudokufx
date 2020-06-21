package net.ladstatt.sudoku

import java.nio.file.{Files, Path, Paths}

import org.bytedeco.opencv.global.{opencv_core, opencv_imgcodecs}
import org.bytedeco.opencv.opencv_core.Mat
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class TemplateLibrarySpec extends AnyWordSpecLike {

  lazy val classicTemplatesFromBmps: Map[Int, Mat] = {
    (for (i <- 1 to 9) yield {
      val p = Paths.get(s"src/main/resources/net/ladstatt/sudoku/templates/classic/$i.bmp")
      i -> opencv_imgcodecs.imread(p.toAbsolutePath.toString, opencv_imgcodecs.IMREAD_GRAYSCALE)
    }).toMap
  }

  def templatePath(i: Int): Path = Paths.get("target").resolve(i.toString + ".bmp")

  def writeTemplates(): Unit = {
    for ((i, m) <- classicTemplatesFromBmps) {
      val t = templatePath(i)
      Files.deleteIfExists(t)
      assert(JavaCV.writeMat(t, m))
    }
  }

  val ts: Seq[Mat] = TemplateLibrary.classicClasspathTemplatesZipped


  "TemplateLibrary" should {
    "contains 9 template definitions" in assert(ts.size == 9)

    /**
     * shows that deserialisation from csv format yields correct images
     *
     * attention to my future self: i've encountered problems writing files here when writing and reading the same file
     * with im_write/im_read
     *
     **/
    "deserializes correctly from csv" ignore {
      writeTemplates()
      val templates = ts.zipWithIndex
      for (o <- 1 to 100) {
        for ((m, i) <- templates) {
          val index = i + 1
          val reference = classicTemplatesFromBmps(index)

          val referencePath = Paths.get("target").resolve(index.toString + "_ref.bmp")
          val diff = new Mat(TemplateLibrary.templateSize, opencv_core.CV_8U)
          opencv_core.compare(reference, m, diff, opencv_core.CMP_EQ)

          assert(classicTemplatesFromBmps(index).asByteBuffer().compareTo(reference.asByteBuffer()) == 0, s"""comparison failed for $index image""")
          Files.deleteIfExists(referencePath)
          assert(JavaCV.writeMat(referencePath, reference))
          val (tArr, rArr) = (Files.readAllBytes(templatePath(index)), Files.readAllBytes(referencePath))
          assert(tArr.sameElements(rArr), s"In run $o, for digit $index, reference was not equal to deserialized data from .csv")
        }
      }
    }
    "detectNumber 1" in {
      val (detectedNr, quality) = TemplateLibrary.detectNumber("SudokuSpec.detectNr", 0, Paths.get("target/"), ts.head)
      assert(1 == detectedNr)
    }

    /**
     * A template matched with itself yields the best results and thus the index of the appropriate number
     */
    "detectNumber works" in {
      for ((m, i) <- ts.zipWithIndex) {
        val (detectedNr, quality) = TemplateLibrary.detectNumber("SudokuSpec.detectNumber", i, Paths.get("target/"), m)
        assert(detectedNr == (i + 1))
      }
    }

  }
}
