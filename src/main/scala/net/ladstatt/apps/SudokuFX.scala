package net.ladstatt.apps

/**
 * Copyright (c) 2013-2014, Robert Ladstätter @rladstaetter
 **/

import _root_.javafx.fxml.{FXML, Initializable}
import _root_.javafx.scene._
import _root_.javafx.scene.control._
import _root_.javafx.scene.paint.Color
import _root_.javafx.stage.Stage
import java.io.{File, FilenameFilter}
import java.net.URL
import java.util.ResourceBundle
import javafx.animation.FadeTransition
import javafx.application.Application
import javafx.beans.property._
import javafx.beans.value.ObservableValue
import javafx.scene.effect.{BlendMode, DropShadow}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{AnchorPane, BorderPane}
import javafx.scene.shape.{Circle, Polyline, Rectangle}

import com.sun.javafx.perf.PerformanceTracker
import net.ladstatt.apps.sudoku._
import net.ladstatt.core.CanLog
import net.ladstatt.jfx.{OpenCVTimedFrameGrabberTask, _}
import net.ladstatt.opencv.OpenCV._
import org.controlsfx.dialog.Dialogs
import org.opencv.core._
import org.opencv.highgui.{Highgui, VideoCapture}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * For a discussion of the concepts of this application see http://ladstatt.blogspot.com/
 */
object SudokuFX {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SudokuFX], args: _*)
  }
}


trait SharedState extends OpenCVJfxUtils with CanLog with JfxUtils {

  import net.ladstatt.apps.sudoku.SudokuAlgos._

  @FXML var canvas: AnchorPane = _

  @FXML var videoView: ImageView = _
  @FXML var solutionButton: ToggleButton = _
  @FXML var viewButtons: ToggleGroup = _
  @FXML var statusLabel: Label = _

  @FXML var bestMatchToolBar: ToolBar = _
  @FXML var nrView1: ImageView = _
  @FXML var nrView2: ImageView = _
  @FXML var nrView3: ImageView = _

  @FXML var nrView4: ImageView = _
  @FXML var nrView5: ImageView = _
  @FXML var nrView6: ImageView = _

  @FXML var nrView7: ImageView = _
  @FXML var nrView8: ImageView = _
  @FXML var nrView9: ImageView = _

  @FXML var templateToolBar: ToolBar = _
  @FXML var tplView1: ImageView = _
  @FXML var tplView2: ImageView = _
  @FXML var tplView3: ImageView = _

  @FXML var tplView4: ImageView = _
  @FXML var tplView5: ImageView = _
  @FXML var tplView6: ImageView = _

  @FXML var tplView7: ImageView = _
  @FXML var tplView8: ImageView = _
  @FXML var tplView9: ImageView = _
  @FXML var historyToolBar: ToolBar = _

  lazy val nrViews: Array[ImageView] =
    Array(nrView1, nrView2, nrView3,
      nrView4, nrView5, nrView6,
      nrView7, nrView8, nrView9)

  def calcFrame(index: Int): Unit

  case class Hit(index: Int, color: Color) extends Rectangle {
    setFill(color)
    setWidth(7)
    setHeight(7)
    setOnMouseEntered(mkEventHandler(e => {
      setScaleX(3)
      setScaleY(3)
      calcFrame(index)
    }))
    setOnMouseExited(mkEventHandler(e => {
      setScaleX(1)
      setScaleY(1)
    }))
  }

  lazy val imageTemplates: Map[Int, Image] = TemplateDetectionStrategy.templateLibrary.map { case (i, m) => i -> toImage(m)}

  def initializeSharedState(location: URL, resources: ResourceBundle): Unit = {
    require(historyToolBar != null)
    require(statusLabel != null)
    require(bestMatchToolBar != null)

    // nr image views
    require(nrView1 != null)
    require(nrView2 != null)
    require(nrView3 != null)

    require(nrView4 != null)
    require(nrView5 != null)
    require(nrView6 != null)

    require(nrView7 != null)
    require(nrView8 != null)
    require(nrView9 != null)

    require(tplView1 != null)
    require(tplView2 != null)
    require(tplView3 != null)

    require(tplView4 != null)
    require(tplView5 != null)
    require(tplView6 != null)

    require(tplView7 != null)
    require(tplView8 != null)
    require(tplView9 != null)

    tplView1.setImage(imageTemplates(1))
    tplView2.setImage(imageTemplates(2))
    tplView3.setImage(imageTemplates(3))
    tplView4.setImage(imageTemplates(4))
    tplView5.setImage(imageTemplates(5))
    tplView6.setImage(imageTemplates(6))
    tplView7.setImage(imageTemplates(7))
    tplView8.setImage(imageTemplates(8))
    tplView9.setImage(imageTemplates(9))

    canvas.getChildren.add(sudokuBorder)
    canvas.getChildren.addAll(analysisCellBounds.toList)
    canvas.getChildren.addAll(analysisCellCorners.toList)
    ()
  }

  val performanceTrackerProperty = new SimpleObjectProperty[PerformanceTracker]()

  def getPerformanceTracker() = performanceTrackerProperty.get()

  def setPerformanceTracker(performanceTracker: PerformanceTracker) = performanceTrackerProperty.set(performanceTracker)

  def updateStatus(message: String, color: Color): Unit = {
    statusLabel.setTextFill(color)
    statusLabel.setText(message)
  }

  def updateBestMatch(sudokuHistory: SudokuState): Unit = {
    // show recognized digits
    execOnUIThread(
      for (i <- Parameters.range) {
        sudokuHistory.digitData(i + 1).map { case m => nrViews(i).setImage(toImage(m))}
      })
  }

  val layoutY = 30.0

  val analysisCellCorners: Array[Circle] = Array.tabulate(100)(mkCellCorner)
  val analysisCellBounds: Array[Polyline] = Array.tabulate(81)(mkCellBound)


  def setAnalysisMouseTransparent(isTransparent: Boolean): Unit = {
    analysisCellCorners.foreach(_.setMouseTransparent(isTransparent))
    analysisCellBounds.foreach(_.setMouseTransparent(isTransparent))
    sudokuBorder.setMouseTransparent(isTransparent)
  }

  def mkCellBound(idx: Int): Polyline = {
    val line = new Polyline()
    val analysisFadeIn = mkFadeTransition(250, line, 0.0, 0.7)
    val analysisFadeOut = mkFadeTransition(700, line, 0.7, 0.0)
    line.setOpacity(0.0)
    line.setFill(Color.LIGHTGOLDENRODYELLOW)
    line.setStroke(Color.LIGHTBLUE)
    line.setStrokeWidth(2)

    line.setBlendMode(BlendMode.MULTIPLY)
    line.setEffect(new DropShadow())
    line.setLayoutY(layoutY)
    line.setOnMouseEntered(mkEventHandler(e => {
      analysisFadeIn.play
      println(s"entered box $idx")
    }))
    line.setOnMouseExited(mkEventHandler(e => {
      analysisFadeOut.play
      println(s"left box $idx")
    }))
    line
  }

  val sudokuBorder = {
    val line = new Polyline()
    line.setStroke(Color.BLUE)
    line.setStrokeWidth(5)
    line.setEffect(new DropShadow())
    line.setLayoutY(layoutY) // 30 pixels down
    line.setOnMouseEntered(mkEventHandler(e => {
      line.setOpacity(1.0)
    }))
    line.setOnMouseExited(mkEventHandler(e => {
      borderFadeTransition.play
    }))
    line
  }

  def mkFadeTransition(time: Double, node: Node, from: Double, to: Double): FadeTransition = {
    val t = new FadeTransition(javafx.util.Duration.millis(time), node)
    t.setFromValue(from)
    t.setToValue(to)
    t
  }

  val borderFadeTransition: FadeTransition = mkFadeTransition(500, sudokuBorder, 1.0, 0.0)

  /**
   * where application will put captured frames
   */
  val workingDirectoryProperty = new SimpleObjectProperty[File](new File("/Users/lad/temp"))

  def getWorkingDirectory = workingDirectoryProperty.get

  def setWorkingDirectory(file: File) = workingDirectoryProperty.set(file)

  def updateVideoView(mat: Mat): Unit = {
    videoView.setImage(toImage(mat))
  }

  def updateDisplay(stage: ProcessingStage, sudokuState: SudokuState): Unit = {
    //  solutionButton.setDisable(sudokuState.someResult.isEmpty)
    stage match {
      case InputStage => updateVideoView(sudokuState.frame)
      case GrayedStage => updateVideoView(sudokuState.imageIoChain.grayed)
      case BlurredStage => updateVideoView(sudokuState.imageIoChain.blurred)
      case ThresholdedStage => updateVideoView(sudokuState.imageIoChain.thresholded)
      case InvertedStage => updateVideoView(sudokuState.imageIoChain.inverted)
      case DilatedStage => updateVideoView(sudokuState.imageIoChain.dilated)
      case ErodedStage => updateVideoView(sudokuState.imageIoChain.eroded)
      case SolutionStage if (sudokuState.someResult.isDefined) => for (r <- sudokuState.someResult) updateVideoView(r.solution)
      case SolutionStage if (sudokuState.someResult.isEmpty) => updateVideoView(sudokuState.frame)
      case _ => ???
    }
  }

  def mkRange(a: Double, b: Double, nrCells: Int = 9): Seq[Double] = {
    if (a == b) {
      List.fill(10)(a)
    } else {
      val r = (a to b by ((b - a) / nrCells))
      if (r.size == 9) List.concat(r, List(b)) else r
    }
  }

  def splitRange(ax: Double, ay: Double, bx: Double, by: Double): Seq[(Double, Double)] = {
    mkRange(ax, bx) zip mkRange(ay, by)
  }

  def mkCellCorner(index: Int): Circle = {
    val c = new Circle
    c.setRadius(3)
    c.setStroke(Color.GOLD)
    c.setFill(Color.INDIANRED)
    c.setLayoutY(layoutY)
    c
  }


  /**
   * returns coordinates of the 100 cell corners
   * @param corners
   * @return
   */
  def mkCellCorners(corners: MatOfPoint2f): IndexedSeq[(Double, Double)] = {
    val List(ul, ur, lr, ll) = corners.toList.toList
    val left = splitRange(ul.x, ul.y, ll.x, ll.y)
    val right = splitRange(ur.x, ur.y, lr.x, lr.y)
    if (left.size == 10) {
      if (right.size == 10) {
        (for (i <- 0 to 9) yield {
          splitRange(left(i)._1, left(i)._2, right(i)._1, right(i)._2).toArray
        }).flatten
      } else {
        logError(s"Right column had not 10 points, but ${right.size}. [$ur,$lr]")
        IndexedSeq()
      }
    } else {
      logError(s"Left column had not 10 points, but ${left.size}. [$ul,$ll]")
      IndexedSeq()
    }
  }

  /**
   * reduces 100 corners to 81 boundaries
   * @param cellCorners
   * @return
   */
  def mkCellBounds(cellCorners: IndexedSeq[(Double, Double)]): IndexedSeq[IndexedSeq[java.lang.Double]] = {

    def extractBound(index: Int): IndexedSeq[java.lang.Double] = {
      IndexedSeq(cellCorners(index)._1, cellCorners(index)._2,
        cellCorners(index + 1)._1, cellCorners(index + 1)._2,
        cellCorners(index + 11)._1, cellCorners(index + 11)._2,
        cellCorners(index + 10)._1, cellCorners(index + 10)._2,
        cellCorners(index)._1, cellCorners(index)._2)
    }
    val exclusions = 9 to 79 by 10
    for (i <- 0 to 88 if (!exclusions.contains(i))) yield extractBound(i)
  }

  def updateCellBounds(border: MatOfPoint2f, cellBounds: Array[Polyline]): Unit = {
    val cellCorners = mkCellCorners(border)
    if (cellCorners.size == 100) {
      val boundCoordinates = mkCellBounds(cellCorners)
      if (boundCoordinates.size == 81) {
        for (i <- 0 to 80) {
          val line = cellBounds(i)
          line.getPoints.clear()
          line.getPoints.addAll(boundCoordinates(i))
        }
      } else {
        logError(s"Found ${boundCoordinates.size} bounds, expected 81 (border size: ${border.size})!")
      }
    } else {
      logError(s"Only found ${cellCorners.size} corners, expected 100 (border size: ${border.size})!")
    }
  }

  def updateCellCorners(corners: MatOfPoint2f, cellCorners: Array[Circle]): Unit = {
    mkCellCorners(corners).zipWithIndex.foreach {
      case ((x, y), index) => {
        cellCorners(index).setCenterX(x)
        cellCorners(index).setCenterY(y)
        mkFadeTransition(500, cellCorners(index), 1.0, 0.0).play
      }
    }


  }

  def updateBorder(corners: MatOfPoint2f): Unit = {
    sudokuBorder.getPoints.clear()
    sudokuBorder.getPoints.addAll(convert2PolyLinePoints(corners.toList))
    borderFadeTransition.play
  }

  def display(sudokuState: SudokuState) = execOnUIThread {
    updateDisplay(viewButtons.getSelectedToggle.getUserData.asInstanceOf[ProcessingStage], sudokuState)
    updateBestMatch(sudokuState)
    updateStatus(mkFps(sudokuState.start), Color.GREEN)
    setAnalysisMouseTransparent(false)
    for (success <- sudokuState.someResult) {
      updateBorder(sudokuState.detectedCorners)
      updateCellBounds(sudokuState.detectedCorners, analysisCellBounds)
      updateCellCorners(sudokuState.detectedCorners, analysisCellCorners)
    }
  }


  def mkFps(start: Long) = {
    def mkDuration = {
      val after = System.nanoTime
      (after - start) / 1000000
    }

    s"FPS " + f"${getPerformanceTracker().getAverageFPS}%3.2f" + s" Frame: ${mkDuration} ms"
  }

}

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
    val sudokuState = SudokuState(frameNumber, frameAt, 1, 20)
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
    allFrames.foreach(println)
    println(s"Framesize is ${allFrames.size}")
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


  def persist(mat: Mat, file: File): Future[File] =
    execFuture {
      if (getCameraActive()) {
        logWithTimer(s"Wrote ${file.getAbsolutePath}", {
          if (!Highgui.imwrite(file.getAbsolutePath, mat)) {
            throw new OpenCVException(mat, s"Could not save to file $file")
          } else {
            file
          }
        })
      } else {
        logWarn("Camera has been closed. Scheduled persist process aborted.")
        new File(".")
      }
    }

  /**
   * main event loop in capturing mode
   */
  def processFrame(observableValue: ObservableValue[_ <: SudokuState],
                   oldState: SudokuState,
                   sudokuState: SudokuState): Unit = {

    for {
     // f <- persist(sudokuState.frame, new File(getWorkingDirectory, s"frame${frameNumber}.png"))
      result <- sudokuState.calc
    } display(result)
  }

  def mkCaptureTask = new OpenCVTimedFrameGrabberTask(new VideoCapture(0), mkChangeListener(processFrame))

  val currentFrameGrabberTaskProperty = new SimpleObjectProperty[OpenCVTimedFrameGrabberTask]()

  def getCurrentFrameGrabberTask() = currentFrameGrabberTaskProperty.get

  def setCurrentFrameGrabberTask(task: OpenCVTimedFrameGrabberTask) = {
    if (getCurrentFrameGrabberTask() != null) {
      getCurrentFrameGrabberTask.cancel
    }
    currentFrameGrabberTaskProperty.set(task)
  }

  // TODO replace whole timer stuff with akka actors
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
    frameTimer.schedule(getCurrentFrameGrabberTask(), 0, 50)
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

  def getPerformanceTracker: PerformanceTracker

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
      .masthead("Solve Sudokus (c) @rladstaetter 2013/2014")
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
    ()
  }


  override def start(stage: Stage): Unit = {
    stage.setTitle("SudokuFX - Show me a Sudoku!")

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


