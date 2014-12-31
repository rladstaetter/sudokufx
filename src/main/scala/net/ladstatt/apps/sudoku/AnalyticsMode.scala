package net.ladstatt.apps.sudoku

import java.io.{File, FilenameFilter}
import java.net.URL
import java.util.ResourceBundle
import javafx.beans.property.SimpleListProperty
import javafx.beans.value.ObservableValue
import javafx.fxml.FXML
import javafx.scene.control.{Slider, ToggleButton}

import net.ladstatt.jfx.JfxUtils
import org.opencv.core.Mat
import org.opencv.highgui.Highgui

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by lad on 31.12.14.
 */
trait AnalyticsMode extends JfxUtils with SharedState {

  @FXML var analyticsButton: ToggleButton = _
  @FXML var historySlider: Slider = _

  val frameBuffer = new mutable.HashMap[Int, Mat]()
  val currentFrameFiles = new SimpleListProperty[File](mkObservableList(List()))

  def setCurrentFrameFiles(files: Seq[File]) = {
    currentFrameFiles.clear()
    currentFrameFiles.addAll(files)
  }

  def getCurrentFrameFiles() = currentFrameFiles.get()

  def getFrameAt(index: Int): Mat = {
    if (!frameBuffer.contains(index)) {
      frameBuffer.put(index, Highgui.imread(currentFrameFiles.get(index).getAbsolutePath))
    }
    frameBuffer(index)
  }

  // TODO signature should provide SudokuState
  // TODO move to SudokuAlgos
  def calcFrame(frameNumber: Int): Unit = {
    val frameAt = getFrameAt(frameNumber)
    println(s"About to show $frameNumber (of ${getCurrentFrameFiles().size})")
    val sudokuState = SCandidate(frameNumber, frameAt, 1, 20)
    for (result <- sudokuState.calc) display(result)
  }

  // TODO replace Number with SudokuState
  def processFrameWithNumber(observableValue: ObservableValue[_ <: Number],
                             oldVal: Number,
                             newVal: Number): Unit =
    if (newVal != null) {
      calcFrame(newVal.intValue())
    }


  def initializeAnalytics(location: URL, resources: ResourceBundle): Unit = {
    deletePersistedFrames()
    historySlider.setShowTickMarks(true)
    historySlider.setShowTickLabels(true)
    historySlider.setBlockIncrement(1)
    historySlider.valueProperty().addListener(mkChangeListener(processFrameWithNumber))
    require(analyticsButton != null)
  }

  def initializeSlider(): Unit = {
    frameBuffer.clear
    setCurrentFrameFiles(allFrames)
    historySlider.setMin(0)
    historySlider.setMax((getCurrentFrameFiles.size - 1).toDouble)
  }

  def deIndex(file: File): (Int, File) = {
    val name = file.getName
    (name.substring("frame".length, name.lastIndexOf(".")).toInt, file)
  }

  def allFrames =
    allFramesUnsorted.map(deIndex).sortWith((a, b) => a._1 < b._1).map(_._2).toSeq

  def allFramesUnsorted = getWorkingDirectory.listFiles(new FilenameFilter() {
    def accept(path: File, name: String): Boolean = {
      name.startsWith("frame")
    }
  })

  def deletePersistedFrames(): Unit = {
    allFrames.foreach(_.delete)
    println(s"Deleted ${allFrames.size} frames.")
    ()
  }

  def exitAnalyticsMode(): Unit = {
    // do nothing
  }

}
