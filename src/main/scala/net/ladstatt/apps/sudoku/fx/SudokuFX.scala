package net.ladstatt.apps.sudoku.fx

/**
 * Copyright (c) 2013-2014, Robert Ladstätter @rladstaetter
 **/

import _root_.javafx.fxml.{FXML, Initializable}
import _root_.javafx.scene._
import _root_.javafx.scene.control._
import _root_.javafx.stage.Stage
import java.net.URL
import java.util.ResourceBundle
import javafx.application.Application
import javafx.beans.value.ObservableValue
import javafx.scene.layout.BorderPane

import com.sun.javafx.perf.PerformanceTracker
import net.ladstatt.apps.sudoku._
import org.controlsfx.dialog.Dialogs

/**
 * For a discussion of the concepts of this application see http://ladstatt.blogspot.com/
 */
object SudokuFX {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SudokuFX], args: _*)
  }
}

class SudokuFX extends Application
with CapturingMode
with AnalyticsMode
with Initializable {

  @FXML var captureButton: ToggleButton = _
  @FXML var inputButton: ToggleButton = _
  @FXML var grayedButton: ToggleButton = _
  @FXML var blurredButton: ToggleButton = _
  @FXML var thresholdedButton: ToggleButton = _
  @FXML var invertedButton: ToggleButton = _
  @FXML var dilatedButton: ToggleButton = _
  @FXML var erodedButton: ToggleButton = _


  def showAbout(): Unit = {
    Dialogs.create()
      .title("About SudokuFx")
      .masthead("Solve Sudokus (c) @rladstaetter 2013/2014/2015")
      .message("Use this application to solve Sudokus.")
      .showInformation()
    ()
  }

  def startAnalytics(): Unit = {
    initializeSlider()
    bestMatchToolBar.setVisible(true)
    templateToolBar.setVisible(true)
  }

  def stopAnalytics(): Unit = {
    deletePersistedFrames()
  }

  def onModeChange(observableValue: ObservableValue[_ <: Toggle],
                   oldValue: Toggle,
                   newValue: Toggle): Unit = {
    if (newValue != null &&
      oldValue != null &&
      oldValue.getUserData != newValue.getUserData) {
      // if there is no new value, ignore
      newValue.getUserData match {
        case CaptureMode => {
          stopAnalytics()
          startCapture()
          println("Capture Mode")
        }
        case AnalyticsMode => {
          stopCapture()
          startAnalytics()
          println("Analytics Mode")
        }
        case x => println(x.getClass)
      }
    }
  }

  sealed trait AppMode

  case object CaptureMode extends AppMode

  case object AnalyticsMode extends AppMode

  override def initialize(location: URL, resources: ResourceBundle): Unit = {

    import net.ladstatt.apps.sudoku.SudokuAlgos._

    initializeSharedState(location, resources)
    initializeAnalytics(location, resources)
    initializeCapturing(location, resources)

    // toggleButtons
    require(inputButton != null)
    require(grayedButton != null)
    require(blurredButton != null)
    require(thresholdedButton != null)
    require(invertedButton != null)
    require(dilatedButton != null)
    require(erodedButton != null)

    require(captureButton != null)
    require(analyticsButton != null)

    captureButton.setUserData(CaptureMode)
    analyticsButton.setUserData(AnalyticsMode)

    modeButtons.selectedToggleProperty.addListener(mkChangeListener(onModeChange))

    inputButton.setUserData(InputStage)
    grayedButton.setUserData(GrayedStage)
    blurredButton.setUserData(BlurredStage)
    thresholdedButton.setUserData(ThresholdedStage)
    invertedButton.setUserData(InvertedStage)
    dilatedButton.setUserData(DilatedStage)
    solutionButton.setUserData(SolutionStage)
    erodedButton.setUserData(ErodedStage)


    startCapture
  }


  override def start(stage: Stage): Unit = {
    stage.setTitle("SudokuFX")

    val scene = new Scene(mk[BorderPane](mkFxmlLoader("/net/ladstatt/apps/sudokufx.fxml", this)))
    setPerformanceTracker(PerformanceTracker.getSceneTracker(scene))
    stage.setScene(scene)
    stage.setOnCloseRequest(mkEventHandler(e => exitApp(stage)))

    stage.show
  }

  def exitApp(stage: Stage): Unit = {
    exitAnalyticsMode()
    exitCapturingMode()
    stage.close()
  }

}


