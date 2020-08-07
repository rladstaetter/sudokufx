package net.ladstatt.sudoku

import java.net.URL
import java.nio.file.{Files, Path}
import java.util.ResourceBundle
import java.util.concurrent.TimeUnit

import _root_.javafx.fxml.{FXML, Initializable}
import _root_.javafx.scene.control._
import javafx.beans.binding.Bindings
import javafx.beans.property.{SimpleIntegerProperty, SimpleLongProperty, SimpleObjectProperty}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.image._
import javafx.scene.layout.FlowPane
import org.bytedeco.javacv.OpenCVFrameGrabber
import org.bytedeco.opencv.opencv_core.Mat
import rx.lang.scala.{Observable, Subscriber}

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


class SudokuFXController extends Initializable with JfxUtils {


  lazy val openCVGrabberProperty = new SimpleObjectProperty[OpenCVFrameGrabber](new OpenCVFrameGrabber(0))

  def setOpenCVGrabberProperty(grabber: OpenCVFrameGrabber): Unit = openCVGrabberProperty.set(grabber)

  def getOpenCVGrabberProperty(): OpenCVFrameGrabber = openCVGrabberProperty.get()

  val imageInputProperty = new SimpleObjectProperty[ImageInput]()

  private def setImageInput(imageInput: ImageInput): Unit = imageInputProperty.set(imageInput)

  def getImageInput() = imageInputProperty.get()


  val sessionsPathProperty = new SimpleObjectProperty[Path]()

  def setSessionsPath(targetPath: Path): Unit = sessionsPathProperty.set(targetPath)

  def getSessionPath(): Path = sessionsPathProperty.get.resolve(getSession.toString)

  val sessionProperty = new SimpleLongProperty()

  def setSession(session: Long): Unit = sessionProperty.set(session)

  def getSession(): Long = sessionProperty.get()

  def nextSessionNumber(sessionsPath: Path): Long = {
    Files.list(sessionsPath.toAbsolutePath).count + 1
  }

  @FXML var numberFlowPane: FlowPane = _

  @FXML var videoView: ImageView = _
  @FXML var normalizedView: ImageView = _
  @FXML var solutionView: ImageView = _

  // current digits which are to be displayed
  @FXML var digitToolBar: ToolBar = _
  @FXML var commands: ToolBar = _
  @FXML var inputChannelChoiceBox: ChoiceBox[ImageInput] = _

  @FXML var startButton: Button = _
  @FXML var stopButton: Button = _
  @FXML var webcamButton: ToggleButton = _
  @FXML var fsButton: ToggleButton = _
  @FXML var showNormalizedButton: ToggleButton = _
  @FXML var showResultButton: ToggleButton = _
  @FXML var showLibraryButton: ToggleButton = _


  @FXML def activateCamera(): Unit = setImageInput(FromVideo)

  @FXML def useFileSystem(): Unit = setImageInput(FromFile)

  @FXML def startProcessing(): Unit = {
    getImageInput() match {
      case FromFile =>
        mkObservable(loadSession(getSessionPath().getParent, getSession()).subscribe)(Duration(1000, TimeUnit.MILLISECONDS))
          .subscribe(
            onResult,
            t => t.printStackTrace(),
            () => {
              logInfo("File stream stopped...")
            })
      case FromVideo =>
        getOpenCVGrabberProperty().start()
        mkObservable(fromWebCam(getOpenCVGrabberProperty()))(Duration(250, TimeUnit.MILLISECONDS))
          .subscribe(
            onResult,
            t => t.printStackTrace(),
            () => {
              logInfo("Videostream stopped...")
            })
    }

  }

  @FXML def stopProcessing(): Unit = {
    getImageInput() match {
      case FromFile =>
        fsButton.setSelected(false)
      case FromVideo =>
        webcamButton.setSelected(false)
        // getOpenCVGrabberProperty().stop()
      case _ =>
    }

  }

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

  def getCameraActive: Boolean = webcamButton.isSelected


  /**
   * reads images from given framegrabber
   *
   * @param subscriber
   */
  def fromWebCam(frameGrabber: OpenCVFrameGrabber)(subscriber: Subscriber[Mat]): Unit = {
    new Thread(
      () => {
        while (getCameraActive) {
          Try(SudokuFXApp.javaCvConverter.convert(frameGrabber.grab())) match {
            case Success(m) =>
              println("width:" + m.size().width())
              println("height:" + m.size.height)
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

  def loadSession(sessionsPath: Path, session: Long): SSession = {
    SSession(s"session $session", sessionsPath.resolve(session.toString))
  }

  def mkObservable(fn: Subscriber[Mat] => Unit)(delay : Duration): Observable[SudokuEnvironment] = {
    Observable[Mat](fn).zipWithIndex.map {
      case (frame, index) =>
        JavaCV.writeMat(getSessionPath().resolve(s"frame-$index.png"), frame)
        // can be null if:
        // - first call
        // - last solution was reset by user
        Option(getSolvedSudoku) match {
          case None =>
            // create default Sudoku instance and fill it with current frame
            SudokuEnvironment("sudoku", index, frame, Seq[Float](), ContourParams(), SudokuState(), getDigitLibrary, None, Seq(), getSessionPath())
          case Some(lastSolution) =>
            SudokuEnvironment("sudoku", index, frame, Seq[Float](), ContourParams(), lastSolution.sudokuState, getDigitLibrary, Option(lastSolution.frameWithSolution), Seq(), getSessionPath())
        }
    } .delaySubscription(delay)
  }


  var cnt: Int = 100

  def onResult(env: SudokuEnvironment): Unit = {
    displayDigitLibrary(env.digitLibrary)
    env.optSudoku match {
      case None =>
        //logTrace("No sudoku / rectangle found ... ")
        setVideoView(env.grayed)
      case Some(sudoku) =>
        // update libary, set global property
        setDigitLibrary(sudoku.digitLibrary)
        if (showNormalizedButton.isSelected) {
          setNormalizedView(JavaCV.copyMat(sudoku.normalized))
        } else {
          // todo hide normalized view
        }
        // todo: if sudoku is already solved, we would just have to apply warp
        // transformations to current image
        if (sudoku.isSolved) {
          cnt = cnt - 1
          //  println("count:" + cnt)
          if (cnt <= 0) {
            cnt = 100
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
            if (showResultButton.isSelected) {
              solvedSudoku.optCNormalized.foreach(setSolutionView)
            }
            setSolvedSudoku(solvedSudoku)
          case None =>
            logTrace("Could not solve sudoku, resetting sudoku state.")
            setVideoView(env.inverted)
            clearSolution()
        }
    }

  }

  def shutdown(): Unit = {
    getOpenCVGrabberProperty().release()
  }

  def initializeCapturing(): Unit = {
    require(Option(videoView).isDefined)
    require(Option(normalizedView).isDefined)
    require(Option(solutionView).isDefined)
  }

  def setVideoView(mat: Mat): Unit = videoView.setImage(JavaCVPainter.toImage(mat))

  def setNormalizedView(mat: Mat): Unit = normalizedView.setImage(JavaCVPainter.toImage(mat))

  def setSolutionView(mat: Mat): Unit = solutionView.setImage(JavaCVPainter.toImage(mat))

  def initStartStop(): Unit = {
    startButton.disableProperty.bind(Bindings.or(webcamButton.selectedProperty(), fsButton.selectedProperty()).not())
    stopButton.disableProperty.bind(Bindings.or(webcamButton.selectedProperty(), fsButton.selectedProperty()).not())
  }

  override def initialize(location: URL, resources: ResourceBundle): Unit = {

    initializeCapturing()

    initDigitToolbar()

    initStartStop()
/*
    imageInputProperty.addListener(new ChangeListener[ImageInput] {
      override def changed(observableValue: ObservableValue[_ <: ImageInput], oldVal: ImageInput, newVal: ImageInput): Unit = {
        ((Option(oldVal), newVal) match {
          // start with from file
          case (None, FromFile) => mkObservable(loadSession(getSessionPath().getParent, getSession()).subscribe)
          case (None, FromVideo) =>
            getOpenCVGrabberProperty().start()
            mkObservable(fromWebCam(getOpenCVGrabberProperty()))
          case (Some(FromFile), FromVideo) =>
            getOpenCVGrabberProperty().start()
            mkObservable(fromWebCam(getOpenCVGrabberProperty()))
          case (Some(FromFile), FromFile) =>
            mkObservable(loadSession(getSessionPath().getParent, getSession()).subscribe)
          case (Some(FromVideo), FromFile) =>
            getOpenCVGrabberProperty().stop()
            mkObservable(loadSession(getSessionPath().getParent, getSession()).subscribe)
          case (Some(FromVideo), FromVideo) =>
            getOpenCVGrabberProperty().start()
            mkObservable(fromWebCam(getOpenCVGrabberProperty()))
        }).subscribe(
          onResult,
          t => t.printStackTrace(),
          () => {
            logInfo("Videostream stopped...")
          })
      }
    })*/

    initCommandsToolbar()

  }

  private def initCommandsToolbar(): Unit = {
    inputChannelChoiceBox.getItems.addAll(FromVideo, FromFile)
    inputChannelChoiceBox.setConverter(new ImageInputStringConverter)

    inputChannelChoiceBox.getSelectionModel.selectedItemProperty.addListener(new ChangeListener[ImageInput] {
      override def changed(observable: ObservableValue[_ <: ImageInput], oldValue: ImageInput, newValue: ImageInput): Unit = {
        // setImageInput(newValue)
      }
    })

    // inputChannelChoiceBox.setValue(FromFile)
  }

  private def initDigitToolbar(): Unit = {
    for (_ <- 0 to 9) {
      digitToolBar.getItems.add(new ImageView)
    }
  }

  def asImage(m: Mat): Image = JavaCVPainter.toImage(m)

  def displayDigitLibrary(digitLibrary: DigitLibrary): Unit = {
    for ((i, DigitEntry(detectedValue, timestamp, quality, optM)) <- digitLibrary.digits) {
      optM match {
        case None => logError(s"No data found for digit $i")
        case Some(m) =>
          //println("quality:" + quality)
          if (showResultButton.isSelected) {
            val image = asImage(m)
            digitToolBar.setVisible(true)
            digitToolBar.getItems.get(i).asInstanceOf[ImageView].setImage(image)
          } else {
            if (digitToolBar.isVisible) {
              digitToolBar.setVisible(false)
            }
          }
      }
    }
  }

}
