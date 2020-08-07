package net.ladstatt.sudoku

/**
 * Copyright (c) 2013-2016, Robert Ladstätter @rladstaetter
 **/

import java.nio.file.{Files, Path, Paths}

import _root_.javafx.scene._
import _root_.javafx.stage.Stage
import javafx.application.Application
import javafx.scene.layout.BorderPane
import org.bytedeco.javacv.OpenCVFrameConverter

import scala.jdk.CollectionConverters
import scala.util.{Failure, Success, Try}

/**
 * For a discussion of the concepts and history of this application see http://ladstatt.blogspot.com/
 */
object SudokuFXApp {

  val javaCvConverter = new OpenCVFrameConverter.ToMat

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SudokuFX], args: _*)
  }
}

class SudokuFX extends Application with JfxUtils {

  import CollectionConverters._

  override def start(stage: Stage): Unit =
    Try {
      stage.setTitle("SudokuFX")
      val params = getParameters.getRaw.asScala
      val sessionsPath = Paths.get(params.headOption.getOrElse("target/sessions"))
      Files.createDirectories(sessionsPath)
      val fxmlLoader = mkFxmlLoader("/net/ladstatt/sudoku/sudokufx.fxml")
      val parent = fxmlLoader.load[BorderPane]()
      val controller = fxmlLoader.getController[SudokuFXController]

      controller.setSessionsPath(sessionsPath)
      if (params.size == 2) {
        val sessionNr = params.tail.head
        controller.setSession(sessionNr.toLong)
        //controller.setImageInput(FromFile)
      } else {
        startVideo(sessionsPath, controller)
      }

      val scene = new Scene(parent)
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

  private def startVideo(sessionsPath: Path, controller: SudokuFXController): Unit = {
    //controller.setSession(controller.nextSessionNumber(sessionsPath))
    //controller.setImageInput(FromVideo)
  }


}




