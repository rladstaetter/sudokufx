package net.ladstatt.sudoku

import java.net.URL
import java.nio.file.{Files, Path}
import java.util.ResourceBundle
import java.util.concurrent.TimeUnit
import java.util.stream.Collectors

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

import scala.concurrent.duration.{Duration, _}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class SudokuFXController extends Initializable with JfxUtils {

  lazy val openCVGrabberProperty = new SimpleObjectProperty[OpenCVFrameGrabber](new OpenCVFrameGrabber(0))
  val imageInputProperty = new SimpleObjectProperty[ImageInput]()
  val sessionsPathProperty = new SimpleObjectProperty[Path]()
  val sessionProperty = new SimpleLongProperty()

  @FXML var numberFlowPane: FlowPane = _

  @FXML var videoView: ImageView = _
  @FXML var normalizedView: ImageView = _
  @FXML var solutionView: ImageView = _

  // current digits which are to be displayed
  @FXML var digitToolBar: ToolBar = _
  @FXML var commands: ToolBar = _
  @FXML var sessionChoiceBox: ChoiceBox[ReplaySession] = _

  @FXML var startButton: Button = _
  @FXML var stopButton: Button = _
  @FXML var webcamButton: ToggleButton = _
  @FXML var fsButton: ToggleButton = _
  @FXML var showNormalizedButton: ToggleButton = _
  @FXML var showResultButton: ToggleButton = _
  @FXML var showLibraryButton: ToggleButton = _

  /** if set, sudokufx will write debug files / images */
  @FXML var writeDebugCheckBox: CheckBox = _

  @FXML def activateCamera(): Unit = setImageInput(FromVideo)

  @FXML def useFileSystem(): Unit = setImageInput(FromFile)

  @FXML def startProcessing(): Unit = {
    getImageInput() match {
      case FromFile =>
        if (sessionChoiceBox.getItems.size() != 0) {
          mkObservable(sessionChoiceBox.getValue.session.subscribe)(Duration(1000, TimeUnit.MILLISECONDS))
            .subscribe(
              onResult,
              t => t.printStackTrace(),
              () => {
                logInfo("File stream stopped...")
              })
          ()
        } else {
          logError("No sessions found.")
        }
      case FromVideo =>
        getOpenCVGrabberProperty().start()
        setSession(nextSessionNumber(getSessionsPath()))
        mkObservable(fromWebCam(getOpenCVGrabberProperty()))(Duration(250, TimeUnit.MILLISECONDS))
          .subscribe(
            onResult,
            t => t.printStackTrace(),
            () => {
              logInfo("Videostream stopped...")
            })
        ()
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
    SSession(s"session $session", sessionsPath.resolve(session.toString), 0 millis)
  }

  def mkObservable(fn: Subscriber[Mat] => Unit)(delay: Duration): Observable[SudokuEnvironment] = {
    Observable[Mat](fn).zipWithIndex.map {
      case (frame, index) =>
        JavaCV.writeMat(writeDebugCheckBox.isSelected())(getSessionPath().resolve(s"frame-$index.png"), frame)
        // can be null if:
        // - first call
        // - last solution was reset by user
        Option(getSolvedSudoku) match {
          case None =>
            // create default Sudoku instance and fill it with current frame
            SudokuEnvironment(
              persistData = writeDebugCheckBox.isSelected()
              , id = "sudoku"
              , frameNr = index
              , frame = frame
              , corners = Seq[Float]()
              , contourParams = ContourParams()
              , history = SudokuState()
              , digitLibrary = getDigitLibrary
              , someSolutionMat = None
              , resultCells = Seq()
              , sessionPath = getSessionPath())
          case Some(lastSolution) =>
            SudokuEnvironment(
              persistData = writeDebugCheckBox.isSelected()
              , id = "sudoku"
              , frameNr = index
              , frame = frame
              , corners = Seq[Float]()
              , contourParams = ContourParams()
              , history = lastSolution.sudokuState
              , digitLibrary = getDigitLibrary
              , someSolutionMat = Option(lastSolution.frameWithSolution)
              , resultCells = Seq()
              , sessionPath = getSessionPath())
        }
    }.delaySubscription(delay)
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
          //println(sudoku.history.asSudokuString)
          //println()
        } else {
          //  setVideoView(env.frame)
        }
        sudoku.trySolve match {
          case Some(solvedSudoku) =>
            // println(solvedSudoku.sudokuHistory.asSudokuString)
            setVideoView(solvedSudoku.frameWithSolution)
            JavaCV.writeMat(true)(getSessionPath().resolve(s"processed-${solvedSudoku.frameNr}.png"), solvedSudoku.frameWithSolution)
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

    initSessionChoiceBox()

  }

  private def initSessionChoiceBox(): Unit = {

    sessionChoiceBox.setConverter(new ReplaySessionStringConverter)
    sessionChoiceBox.disableProperty().bind(fsButton.selectedProperty().not)

    // if there is a change to the fsButton, namely fsButton is set to 'selectec' recalculate sessions, in order to
    // also get new sessions which might have been recorded in the meantime
    fsButton.selectedProperty().addListener(new ChangeListener[java.lang.Boolean] {
      override def changed(observable: ObservableValue[_ <: java.lang.Boolean], oldValue: java.lang.Boolean, newValue: java.lang.Boolean): Unit = {
        if (newValue) {
          resetSessionsChoiceBox()
        }
      }
    })
    sessionsPathProperty.addListener(new ChangeListener[Path] {
      override def changed(observable: ObservableValue[_ <: Path], oldValue: Path, newValue: Path): Unit = {
        resetSessionsChoiceBox()
      }
    })

  }

  private def resetSessionsChoiceBox(): Unit = {
    sessionChoiceBox.getItems.clear()
    val sessions = ReplaySession.listAll(getSessionsPath()).collect(Collectors.toList())
    sessionChoiceBox.getItems.addAll(sessions)
    if (sessions.size != 0) {
      sessionChoiceBox.setValue(sessions.get(0))
    }
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

  def setOpenCVGrabberProperty(grabber: OpenCVFrameGrabber): Unit = openCVGrabberProperty.set(grabber)

  def getOpenCVGrabberProperty(): OpenCVFrameGrabber = openCVGrabberProperty.get()


  private def setImageInput(imageInput: ImageInput): Unit = imageInputProperty.set(imageInput)

  def getImageInput() = imageInputProperty.get()


  def setSessionsPath(sessionsPath: Path): Unit = sessionsPathProperty.set(sessionsPath)

  def getSessionsPath(): Path = sessionsPathProperty.get()

  def getSessionPath(): Path = sessionsPathProperty.get.resolve(getSession.toString)


  def setSession(session: Long): Unit = sessionProperty.set(session)

  def getSession(): Long = sessionProperty.get()

  def nextSessionNumber(sessionsPath: Path): Long = {
    Files.list(sessionsPath.toAbsolutePath).count + 1
  }
}
