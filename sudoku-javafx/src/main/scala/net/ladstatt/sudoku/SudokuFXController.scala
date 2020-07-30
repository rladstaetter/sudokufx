package net.ladstatt.sudoku

import java.net.URL
import java.nio.file.Path
import java.util.ResourceBundle
import java.util.concurrent.TimeUnit

import _root_.javafx.fxml.{FXML, Initializable}
import _root_.javafx.scene.control._
import javafx.beans.property.{SimpleBooleanProperty, SimpleIntegerProperty, SimpleLongProperty, SimpleObjectProperty}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.image._
import javafx.scene.layout.FlowPane
import org.bytedeco.javacv.OpenCVFrameGrabber
import org.bytedeco.opencv.opencv_core.Mat
import rx.lang.scala.{Observable, Subscriber}

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


class SudokuFXController extends Initializable with JfxUtils {
  val imageInputProperty = new SimpleObjectProperty[ImageInput]()

  def setImageInput(imageInput: ImageInput): Unit = imageInputProperty.set(imageInput)

  def getImageInput() = imageInputProperty.get()

  val sessionProperty = new SimpleLongProperty()

  def setSession(session: Long): Unit = sessionProperty.set(session)

  def getSession(): Long = sessionProperty.get()


  val sessionsPathProperty = new SimpleObjectProperty[Path]()

  def setSessionsPath(targetPath: Path): Unit = sessionsPathProperty.set(targetPath)

  def getSessionPath(): Path = sessionsPathProperty.get.resolve(getSession.toString)

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

  def mkObservable(fn: Subscriber[Mat] => Unit): Observable[SudokuEnvironment] = {
    Observable[Mat](fn).zipWithIndex.map {
      case (frame, index) =>
        //val fName = Sudoku.targetPath.resolve("session").resolve(index.toString + ".png")
        // JavaCV.writeMat(fName, frame)
        JavaCV.writeMat(getSessionPath().resolve(s"frame-$index.png"), frame)
        val params: ContourParams = ContourParams()
        // can be null if :
        // - first call
        // - last solution was reset by user
        Option(getSolvedSudoku) match {
          case None =>
            // create default Sudoku instance and fill it with current frame
            SudokuEnvironment("sudoku", index, frame, Seq[Float](), params, SudokuState(), getDigitLibrary, None, Seq(), getSessionPath())
          case Some(lastSolution) =>
            SudokuEnvironment("sudoku", index, frame, Seq[Float](), params, lastSolution.sudokuState, getDigitLibrary, Option(lastSolution.frameWithSolution), Seq(), getSessionPath())
        }
    }.delaySubscription(Duration(2000, TimeUnit.MILLISECONDS))


  }


  var cnt: Int = 100

  def onResult(env: SudokuEnvironment): Unit = {
    displayDigitLibrary(env.digitLibrary)
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

  def setNormalizedView(mat: Mat): Unit = normalizedView.setImage(JavaCVPainter.toImage(mat))

  def setSolutionView(mat: Mat): Unit = solutionView.setImage(JavaCVPainter.toImage(mat))

  override def initialize(location: URL, resources: ResourceBundle): Unit = {

    initializeCapturing()

    for (_ <- 0 to 9) {
      digitToolBar.getItems.add(new ImageView)
    }

    imageInputProperty.addListener(new ChangeListener[ImageInput] {
      override def changed(observableValue: ObservableValue[_ <: ImageInput], oldVal: ImageInput, newVal: ImageInput): Unit = {
        (newVal match {
          case FromFile =>
            mkObservable(loadSession(getSessionPath().getParent, getSession()).subscribe)
          case FromVideo =>
            val grabber = new OpenCVFrameGrabber(0)
            grabber.start()
            mkObservable(fromWebCam(grabber))
        }).subscribe(
          onResult,
          t => t.printStackTrace(),
          () => {
            logInfo("Videostream stopped...")
            System.exit(0)
          })
      }
    })

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
