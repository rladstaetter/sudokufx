package net.ladstatt.sudoku

/**
 * Copyright (c) 2013-2016, Robert Ladstätter @rladstaetter
 **/

import _root_.javafx.scene._
import _root_.javafx.stage.Stage
import com.sun.javafx.perf.PerformanceTracker
import javafx.application.Application
import javafx.scene.layout.VBox

import scala.util.{Failure, Success, Try}

/**
 * For a discussion of the concepts and history of this application see http://ladstatt.blogspot.com/
 */
object SudokuFXApp {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SudokuFX], args: _*)
  }
}

class SudokuFX extends Application with JfxUtils {

  override def start(stage: Stage): Unit =
    Try {
      stage.setTitle("SudokuFX")

      val fxmlLoader = mkFxmlLoader("/net/ladstatt/sudoku/sudokufx.fxml")
      val parent = fxmlLoader.load[VBox]()
      val controller = fxmlLoader.getController[SudokuFXController]
      val scene = new Scene(parent)
      controller.setPerformanceTracker(PerformanceTracker.getSceneTracker(scene))
      stage.setScene(scene)

      stage.setOnCloseRequest(mkEventHandler(_ => {
        controller.shutdown()
        stage.close()
      }))
      stage.show()

    } match {
      case Success(_) =>
      case Failure(e) =>
        e.printStackTrace()
        System.err.println("Could not initialize SudokuFX application.")

    }

}




