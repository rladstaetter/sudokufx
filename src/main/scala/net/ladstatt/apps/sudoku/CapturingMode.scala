package net.ladstatt.apps.sudoku

import java.net.URL
import java.util.ResourceBundle
import javafx.beans.property.{SimpleBooleanProperty, SimpleObjectProperty}
import javafx.beans.value.ObservableValue
import javafx.fxml.FXML
import javafx.scene.control.ToggleGroup
import javafx.scene.paint.Color
import net.ladstatt.jfx.{FrameGrabberTask, FrameTimer, JfxUtils, OpenCVJfxUtils}
import net.ladstatt.opencv.OpenCV._
import scala.concurrent.ExecutionContext.Implicits.global
/**
 * Created by lad on 31.12.14.
 */
trait CapturingMode extends JfxUtils
with OpenCVJfxUtils
with SharedState {

  @FXML var modeButtons: ToggleGroup = _

  loadNativeLib()

  def updateHistoryToolbar(frameNumber: Int, color: Color): Unit =
    execOnUIThread {
      println(s"adding $frameNumber with $color")
      historyToolBar.getItems.add(Hit(frameNumber, color))
      ()
    }

  /**
   * main event loop in capturing mode
   */
  def processFrame(observableValue: ObservableValue[_ <: SCandidate],
                   oldState: SCandidate,
                   sudokuCandidate: SCandidate): Unit = {
    val updatedState: SCandidate =
      if (oldState != null) {
        sudokuCandidate.copy(hCounts = oldState.hCounts.clone(),
          digitQuality = oldState.digitQuality.clone(),
          digitData = oldState.digitData.clone)
      }
      else sudokuCandidate
    for {
    // f <- persist(sudokuCandidate.frame, new File(getWorkingDirectory, s"frame${sudokuCandidate.nr}.png"))
      _ <- sudokuCandidate.persistFrame(getWorkingDirectory)
      result <- updatedState.calc
    } display(result)
  }


  def mkCaptureTask = new FrameGrabberTask(processFrame)

  val currentFrameGrabberTaskProperty = new SimpleObjectProperty[FrameGrabberTask]()

  def getCurrentFrameGrabberTask = currentFrameGrabberTaskProperty.get

  def setCurrentFrameGrabberTask(task: FrameGrabberTask) = {
    if (getCurrentFrameGrabberTask != null) {
      getCurrentFrameGrabberTask.cancel
    }
    currentFrameGrabberTaskProperty.set(task)
  }

  val frameTimer = new FrameTimer

  val cameraActiveProperty = new SimpleBooleanProperty(false)

  def setCameraActive(isActive: Boolean): Unit = cameraActiveProperty.set(isActive)

  def getCameraActive(): Boolean = cameraActiveProperty.get

  def stopCapture(): Unit = {
    setCameraActive(false)
    logInfo("Stopping camera, no new persist jobs should be triggered.")
    setCurrentFrameGrabberTask(null)
  }

  def startCapture(): Unit = {
    resetHistoryBar()
    setCurrentFrameGrabberTask(mkCaptureTask)
    frameTimer.schedule(getCurrentFrameGrabberTask, 0, 50)
    setCameraActive(true)
    bestMatchToolBar.setVisible(false)
    templateToolBar.setVisible(false)
  }


  def exitCapturingMode(): Unit = {
    stopCapture()
    frameTimer.cancel()
    frameTimer.purge()
    ()
  }

  def initializeCapturing(location: URL, resources: ResourceBundle): Unit = {
    require(viewButtons != null)
    require(modeButtons != null)

    require(videoView != null)
    require(solutionButton != null)

  }


  def resetHistoryBar() = {
    historyToolBar.getItems.clear()
  }


}
