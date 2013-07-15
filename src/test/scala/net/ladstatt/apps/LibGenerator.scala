package net.ladstatt.apps

import java.io.File
import org.opencv.core.CvType
import org.opencv.highgui.Highgui
import java.util.UUID


/**
 * Created by lad on 05.06.13.
 */
object LibGenerator extends Sudokuaner {

  def generateTrainData(testData: SudokuTestData, fileName: String, targetPath: File): Unit = {
    val expected = testData.data(fileName)

    mkSomeSudoku(input = readImage(new File(testData.resPath, fileName), CvType.CV_8UC1),
      detectNumberMethod = withFeatureExtraction(mkComparisonLibrary(testData.trainingPath))) match {
      case Some((warped, corners, cells)) => {
        val mappedCells = (cells.filter(c => c match {
          case s: SudokuCell => true
          case _ => false
        }).asInstanceOf[Seq[SudokuCell]].groupBy {
          case sc => sc.value
        })

        for ((value, scells) <- mappedCells) {
          val path = new File(targetPath, "/%s/".format(value))
          path.mkdirs
          scells.zipWithIndex.map {
            case (sc, i) =>
              Highgui.imwrite(path.getAbsolutePath + "/%s_%s.png".format(value, UUID.randomUUID()), sc.contour.resizedNumberData)
          }
        }
      }
      case None => println("No Sudoku detected.")
    }
  }

  def main(args: Array[String]): Unit = {
    val loadLibs = loadNativeLibs()
    val inputs = (1 to KleineZeitung.data.size).map("sudoku%s.png".format(_))
    inputs.map(generateTrainData(KleineZeitung, _, new File("target/kleinezeitung")))
  }
}