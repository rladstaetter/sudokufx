package net.ladstatt.sudoku

import java.net.URL
import java.nio.file.Paths
import java.util.ResourceBundle
import java.util.concurrent.TimeUnit

import _root_.javafx.fxml.{FXML, Initializable}
import _root_.javafx.scene._
import _root_.javafx.scene.control._
import javafx.animation.FadeTransition
import javafx.beans.property.{SimpleBooleanProperty, SimpleIntegerProperty, SimpleObjectProperty}
import javafx.scene.effect.{BlendMode, DropShadow}
import javafx.scene.image._
import javafx.scene.layout.FlowPane
import javafx.scene.paint.Color
import javafx.scene.shape.Polyline
import org.bytedeco.javacv.OpenCVFrameGrabber
import org.bytedeco.opencv.opencv_core.Mat
import rx.lang.scala.{Observable, Subscriber}

import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}


class SudokuFXController extends Initializable with JfxUtils {

  @FXML var numberFlowPane: FlowPane = _

  @FXML var videoView: ImageView = _
  @FXML var normalizedView: ImageView = _
  @FXML var solutionView: ImageView = _

  // current digits which are to be displayed
  @FXML var digitToolBar: ToolBar = _


  val digitLibraryProperty = new SimpleObjectProperty[DigitLibrary](Parameters.defaultDigitLibrary)
  val solvedSudokuProperty = new SimpleObjectProperty[SolvedSudoku]()

  def setDigitLibrary(m: DigitLibrary): Unit = {
    digitLibraryProperty.set(m)
  }

  def getDigitLibrary: DigitLibrary = digitLibraryProperty.get()

  def clearSolution(): Unit = {
    setDigitLibrary(Parameters.defaultDigitLibrary)
    setSolvedSudoku(null)
  }

  def setSolvedSudoku(solution: SolvedSudoku): Unit = solvedSudokuProperty.set(solution)

  def getSolvedSudoku: SolvedSudoku = solvedSudokuProperty.get()


  val frameNumberProperty = new SimpleIntegerProperty(this, "frameNumberProperty", 0)

  def getFrameNumber: Int = frameNumberProperty.get()

  def setFrameNumber(i: Int): Unit = frameNumberProperty.set(i)


  val cameraActiveProperty = new SimpleBooleanProperty(true)

  def setCameraActive(isActive: Boolean): Unit = cameraActiveProperty.set(isActive)

  def getCameraActive: Boolean = cameraActiveProperty.get

  val frameGrabber: OpenCVFrameGrabber = {
    val grabber = new OpenCVFrameGrabber(0)
    grabber.start()
    grabber
  }

  /**
   * reads images from a webcam
   *
   * @param subscriber
   */
  def fromWebCam(subscriber: Subscriber[Mat]): Unit = {
    new Thread(
      () => {
        while (getCameraActive) {
          Try(SudokuFXApp.javaCvConverter.convert(frameGrabber.grab())) match {
            case Success(m) =>

              subscriber.onNext(m)
            case Failure(e) =>
              e.printStackTrace()
              subscriber.onError(e)
          }
        }
        logInfo("Shutting down video service ... ")
        frameGrabber.release()
        subscriber.onCompleted()
      }).start()
  }


  val s1 = SSession("session 1", Paths.get("/Users/lad/Documents/sudokufx/sudoku-core/src/test/resources/net/ladstatt/sudoku/session1"))
  val s2 = SSession("session 2", Paths.get("/Users/lad/Documents/sudokufx/sudoku-core/src/test/resources/net/ladstatt/sudoku/session2"))


  def mkObservable(fn: Subscriber[Mat] => Unit): Observable[SudokuEnvironment] = {
    Observable[Mat](fn).zipWithIndex.map {
      case (frame, index) =>
        //val fName = Sudoku.targetPath.resolve("session").resolve(index.toString + ".png")
        // JavaCV.writeMat(fName, frame)
        JavaCV.writeMat(Sudoku.targetPath.resolve(s"frame-$index.png"), frame)
        val params: ContourParams = ContourParams()
        // can be null if :
        // - first call
        // - last solution was reset by user
        Option(getSolvedSudoku) match {
          case None =>
            // , create default Sudoku instance and fill it with current frame
            SudokuEnvironment(s"sudoku", index, frame, Seq[Float](), params, SudokuState(), getDigitLibrary)
          case Some(lastSolution) =>
            // provide history of search such that we can differ between cells which are always identified
            // and those who aren't. By choosing those cells with the most 'stable' configuration we assume
            // that image recognition provided correct results
            SudokuEnvironment("sudoku", index, frame, Seq[Float](), params, lastSolution.sudokuHistory, getDigitLibrary)
        }
    }.delaySubscription(Duration(2000, TimeUnit.MILLISECONDS))


  }


  val envObservable: Observable[SudokuEnvironment] = {
    if (Sudoku.useTestSession) {
      mkObservable(s2.subscribe)
    } else mkObservable(fromWebCam)
  }

  var cnt: Int = 100

  def onResult(env: SudokuEnvironment): Unit = {
    displayDigitLibrary(env.library)
    env.optSudoku match {
      case None =>
        //logTrace("No sudoku / rectangle found ... ")
        setVideoView(env.grayed)
        //setNormalizedView(env.dilated)
      case Some(sudoku) =>
        // update libary, set global property
        setDigitLibrary(sudoku.digitLibrary)
        setNormalizedView(JavaCV.copyMat(sudoku.normalized))
        // todo: if sudoku is already solved, we would just have to apply warp
        // transformations to current image
        if (sudoku.isSolved) {
          cnt = cnt - 1
          if (cnt == 0) {
            cnt = 1000
            clearSolution()
          }
          println(sudoku.history.asSudokuString)
          println()
        } else {
          setVideoView(env.frame)
        }
        sudoku.trySolve match {
          case Some(solvedSudoku) =>
            // println(solvedSudoku.sudokuHistory.asSudokuString)
            setVideoView(solvedSudoku.frameWithSolution)
            // TODO triggers exception
            solvedSudoku.optCNormalized.foreach(setSolutionView)
            setSolvedSudoku(solvedSudoku)
          case None =>
            logTrace("Could not solve sudoku, resetting sudoku state.")
            setVideoView(env.inverted)
            clearSolution()
        }
    }

  }

  def shutdown(): Unit = {
    setCameraActive(false)
  }

  def initializeCapturing(): Unit = {
    require(Option(videoView).isDefined)
    require(Option(normalizedView).isDefined)
    require(Option(solutionView).isDefined)
  }

  def setVideoView(mat: Mat): Unit = videoView.setImage(JavaCVPainter.toImage(mat))

  def setNormalizedView(mat: Mat): Unit =  normalizedView.setImage(JavaCVPainter.toImage(mat))

  def setSolutionView(mat: Mat): Unit =  solutionView.setImage(JavaCVPainter.toImage(mat))

  override def initialize(location: URL, resources: ResourceBundle): Unit = {

    initializeCapturing()

    for (_ <- 0 to 9) {
      digitToolBar.getItems.add(new ImageView)
    }

    // startCapture
    envObservable.subscribe(
      onResult,
      t => t.printStackTrace(),
      () => {
        logInfo("Videostream stopped...")
        System.exit(0)
      })
    ()
  }

  def asImage(m: Mat): Image = JavaCVPainter.toImage(m)

  def displayDigitLibrary(digitLibrary: DigitLibrary): Unit = {
    for ((i, DigitEntry(detectedValue, timestamp, quality, optM)) <- digitLibrary.digits) {
      optM match {
        case None => logError(s"No data found for digit $i")
        case Some(m) =>
          println("quality:" + quality)
          val image = asImage(m)
          digitToolBar.setVisible(true)
          digitToolBar.getItems.get(i).asInstanceOf[ImageView].setImage(image)
      }
    }
  }

}
