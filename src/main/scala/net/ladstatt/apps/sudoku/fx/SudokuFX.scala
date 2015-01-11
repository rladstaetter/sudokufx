package net.ladstatt.apps.sudoku.fx

/**
 * Copyright (c) 2013-2014, Robert Ladstätter @rladstaetter
 **/

import _root_.javafx.fxml.{FXML, Initializable}
import _root_.javafx.scene._
import _root_.javafx.scene.control._
import _root_.javafx.stage.Stage
import java.io.File
import java.net.URL
import java.text.SimpleDateFormat
import java.util.{Date, ResourceBundle}
import javafx.animation.FadeTransition
import javafx.application.Application
import javafx.beans.property.{SimpleBooleanProperty, SimpleIntegerProperty, SimpleObjectProperty}
import javafx.beans.value.ObservableValue
import javafx.geometry.Pos
import javafx.scene.effect.{BlendMode, DropShadow}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{AnchorPane, BorderPane, FlowPane}
import javafx.scene.paint.Color
import javafx.scene.shape.{Circle, Polyline, Rectangle}

import com.sun.javafx.perf.PerformanceTracker
import net.ladstatt.apps.sudoku.Parameters._
import net.ladstatt.apps.sudoku._
import net.ladstatt.core.CanLog
import net.ladstatt.jfx.{FrameGrabberTask, FrameTimer, JfxUtils, OpenCVJfxUtils}
import net.ladstatt.opencv.OpenCV._
import org.controlsfx.dialog.Dialogs
import org.opencv.core.{Mat, Point}

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * For a discussion of the concepts of this application see http://ladstatt.blogspot.com/
 */
object SudokuFX {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SudokuFX], args: _*)
  }
}

class SudokuFX extends Application with Initializable with OpenCVJfxUtils with CanLog with JfxUtils {

  import net.ladstatt.apps.sudoku.SudokuAlgos._

  @FXML var captureButton: ToggleButton = _
  @FXML var inputButton: ToggleButton = _
  @FXML var grayedButton: ToggleButton = _
  @FXML var blurredButton: ToggleButton = _
  @FXML var thresholdedButton: ToggleButton = _
  @FXML var invertedButton: ToggleButton = _
  @FXML var dilatedButton: ToggleButton = _
  @FXML var erodedButton: ToggleButton = _
  @FXML var statsFlowPane: FlowPane = _
  @FXML var resultFlowPane: FlowPane = _

  @FXML var numberFlowPane: FlowPane = _
  @FXML var canvas: AnchorPane = _

  @FXML var videoView: ImageView = _
  @FXML var solutionButton: ToggleButton = _
  @FXML var viewButtons: ToggleGroup = _
  @FXML var statusLabel: Label = _

  @FXML var templateToolBar: ToolBar = _
  @FXML var mainMenuBar: MenuBar = _
  @FXML var modeButtons: ToggleGroup = _

  val currentSudokuStateProperty = new SimpleObjectProperty[SudokuState](SudokuState())
  def getCurrentSudokuState() = currentSudokuStateProperty.get()
  def setCurrentSudokuState(sudokuState : SudokuState) = currentSudokuStateProperty.set(sudokuState)

  val frameNumberProperty = new SimpleIntegerProperty(this, "frameNumberProperty", 0)

  def setFrameNumber(i: Int) = frameNumberProperty.set(i)

  def getFrameNumber() = frameNumberProperty.get()


  val history = new File("runs/").listFiles()

  val historyMenu: Menu = {
    val historyMenu = new Menu("Previous runs")
    val historyItems: Seq[MenuItem] = {
      history.map(f => {
        val i = new MenuItem(f.getName + s" (${f.list().size})")
        i
      })
    }
    historyMenu.getItems.addAll(historyItems)
    historyMenu
  }

  lazy val imageTemplates: Map[Int, Image] = Parameters.templateLibrary.map { case (i, m) => i -> toImage(m)}


  loadNativeLib()
      /*
  def mergeHitCounts(currentHitCounts: HitCounts, hitCounts: HitCounts): HitCounts = {
    for {(as, bs) <- (currentHitCounts zip hitCounts)} yield {
      for {(a, b) <- (as zip bs)} yield {
        a + b
      }
    }
  }

  def duplicate(hitCounts: HitCounts): HitCounts = {
    for {as <- hitCounts} yield {
      for {b <- as} yield b
    }
  }

  def mergeWithCurrent(result: SudokuResult): Unit = {
    setCurrentHitCounts(mergeHitCounts(getCurrentHitCounts, result.candidate.currentState.hCounts))
  }
          */
  /**
   * main event loop in capturing mode
   */
  def processFrame(observableValue: ObservableValue[_ <: Mat],
                   oldFrame: Mat,
                   currentFrame: Mat): Unit = {
    val currentFrameNumber = getFrameNumber()
    setFrameNumber(currentFrameNumber + 1)

    val candidate = SCandidate(nr = currentFrameNumber, frame = currentFrame)

    for {
      _ <- persistFrame(candidate.frame, candidate.nr, getWorkingDirectory)
      result <- candidate.calc(getCurrentSudokuState())
    } {
      display(result)
    }
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
    setCurrentFrameGrabberTask(mkCaptureTask)
    frameTimer.schedule(getCurrentFrameGrabberTask, 0, 50)
    setCameraActive(true)
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
    require(videoView != null)
    require(solutionButton != null)
  }

  def initializeSharedState(location: URL, resources: ResourceBundle): Unit = {
    require(statusLabel != null)

    require(templateToolBar != null)
    require(mainMenuBar != null)

    mainMenuBar.getMenus.add(historyMenu)
    imageTemplates.foldLeft(templateToolBar) {
      case (acc, (i, image)) => {
        templateToolBar.getItems.add(new ImageView(image))
        templateToolBar
      }
    }
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

  def updateBestMatch(currentState: SudokuState, nrViews: Seq[ImageView]): Unit = {
    // show recognized digits
    execOnUIThread(
      for (i <- Parameters.range) {
        currentState.digitData(i + 1).map { case m => nrViews(i).setImage(toImage(m))}
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
      logInfo(s"entered box $idx")
    }))
    line.setOnMouseExited(mkEventHandler(e => {
      analysisFadeOut.play
      logInfo(s"left box $idx")
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
  val basedir = new File("/Users/lad/Documents/sudokufx/runs/")
  val sessionName = new SimpleDateFormat("yyyy-MM-dd-HH-mm").format(new Date())
  val workingDirectory = new File(basedir, sessionName)
  workingDirectory.mkdirs()

  /**
   * where application will put captured frames
   */
  val workingDirectoryProperty = new SimpleObjectProperty[File](workingDirectory)

  def getWorkingDirectory = workingDirectoryProperty.get

  def setWorkingDirectory(file: File) = workingDirectoryProperty.set(file)

  def updateVideoView(mat: Mat): Unit = {
    videoView.setImage(toImage(mat))
  }

  def updateDisplay(stage: ProcessingStage, sudokuResult: SudokuResult): Unit = {

    def updateVideo(stage: ProcessingStage, candidate: SCandidate, solutionMat: Mat): Unit = {
      stage match {
        case InputStage => updateVideoView(candidate.frame)
        case GrayedStage => updateVideoView(candidate.imageIoChain.grayed)
        case BlurredStage => updateVideoView(candidate.imageIoChain.blurred)
        case ThresholdedStage => updateVideoView(candidate.imageIoChain.thresholded)
        case InvertedStage => updateVideoView(candidate.imageIoChain.inverted)
        case DilatedStage => updateVideoView(candidate.imageIoChain.dilated)
        case ErodedStage => updateVideoView(candidate.imageIoChain.eroded)
        case SolutionStage => updateVideoView(solutionMat)
        case _ => ???
      }
    }

    sudokuResult match {
      case SSuccess(candidate, detectedCells, solution, solutionMat, solutionCells) => {
        updateVideo(stage, candidate, solutionMat)
        displayResult(solution, as[Label](resultFlowPane.getChildren))
        displayHitCounts(getCurrentSudokuState.hCounts, as[FlowPane](statsFlowPane.getChildren))
      }
      case SFailure(candidate) => {
        updateVideo(stage, candidate, candidate.frame)
      }
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
  def mkCellCorners(corners: List[Point]): Seq[(Double, Double)] = {
    val List(ul, ur, lr, ll) = corners
    val left = splitRange(ul.x, ul.y, ll.x, ll.y)
    val right = splitRange(ur.x, ur.y, lr.x, lr.y)
    if (left.size == 10) {
      if (right.size == 10) {
        (for (i <- 0 to 9) yield {
          splitRange(left(i)._1, left(i)._2, right(i)._1, right(i)._2).toArray
        }).flatten
      } else {
        logError(s"Right column had not 10 points, but ${right.size}. [$ur,$lr]")
        Seq()
      }
    } else {
      logError(s"Left column had not 10 points, but ${left.size}. [$ul,$ll]")
      Seq()
    }
  }

  /**
   * reduces 100 corners to 81 boundaries
   * @param cellCorners
   * @return
   */
  def mkCellBounds(cellCorners: Seq[(Double, Double)]): Seq[Seq[java.lang.Double]] = {

    def extractBound(index: Int): Seq[java.lang.Double] = {
      IndexedSeq(cellCorners(index)._1, cellCorners(index)._2,
        cellCorners(index + 1)._1, cellCorners(index + 1)._2,
        cellCorners(index + 11)._1, cellCorners(index + 11)._2,
        cellCorners(index + 10)._1, cellCorners(index + 10)._2,
        cellCorners(index)._1, cellCorners(index)._2)
    }
    val exclusions = 9 to 79 by 10
    for (i <- 0 to 88 if (!exclusions.contains(i))) yield extractBound(i)
  }

  def updateCellBounds(border: List[Point], cellBounds: Array[Polyline]): Unit = {
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

  def updateCellCorners(corners: List[Point], cellCorners: Array[Circle]): Unit = {
    mkCellCorners(corners).zipWithIndex.foreach {
      case ((x, y), index) => {
        cellCorners(index).setCenterX(x)
        cellCorners(index).setCenterY(y)
        mkFadeTransition(500, cellCorners(index), 1.0, 0.0).play
      }
    }
  }

  def updateBorder(corners: List[Point]): Unit = {
    sudokuBorder.getPoints.clear()
    sudokuBorder.getPoints.addAll(convert2PolyLinePoints(corners))
    borderFadeTransition.play
  }

  def display(result: SudokuResult) = execOnUIThread {
    result match {
      case success: SSuccess => {
        updateDisplay(viewButtons.getSelectedToggle.getUserData.asInstanceOf[ProcessingStage], result)
        setAnalysisMouseTransparent(false)
        updateBestMatch(getCurrentSudokuState, as[ImageView](numberFlowPane.getChildren))
        updateStatus(mkFps(success.candidate.start), Color.GREEN)
        if (success.candidate.sudokuCellDetector.foundCorners) {
          updateBorder(success.sudokuCorners)
          updateCellBounds(success.sudokuCorners, analysisCellBounds)
          updateCellCorners(success.sudokuCorners, analysisCellCorners)
        }
      }
      case SFailure(candidate) => {
        updateDisplay(viewButtons.getSelectedToggle.getUserData.asInstanceOf[ProcessingStage], result)
        setAnalysisMouseTransparent(false)
        updateBestMatch(getCurrentSudokuState, as[ImageView](numberFlowPane.getChildren))
        updateStatus(mkFps(candidate.start), Color.GREEN)
      }
    }
  }


  def mkFps(start: Long) = {
    def mkDuration = {
      val after = System.nanoTime
      (after - start) / 1000000
    }

    s"FPS " + f"${getPerformanceTracker().getAverageFPS}%3.2f" + s" Frame: ${mkDuration} ms"
  }

  def showAbout(): Unit = {
    Dialogs.create()
      .title("About SudokuFx")
      .masthead("Solve Sudokus (c) @rladstaetter 2013/2014/2015")
      .message("Use this application to solve Sudokus.")
      .showInformation()
    ()
  }

  def initResultPane(resultPane: FlowPane): Boolean = {
    def mkSolutionPane(): Seq[Label] = {
      val cells = 1 to 81
      for (c <- cells) yield {
        val l = new Label("0")
        l.setAlignment(Pos.CENTER)
        l.setStyle("-fx-border-color:yellow;-fx-border-width:1px;-fx-border-style:solid")
        l.setMinHeight(20.0)
        l.setMaxHeight(20.0)
        l.setMinWidth(20.0)
        l.setMaxWidth(20.0)
        l
      }
    }
    resultPane.setPrefWrapLength(20.0 * 9)
    resultPane.setMaxWidth(20.0 * 9)
    resultPane.getChildren.addAll(mkSolutionPane())
  }

  def initStatsPane(flowPane: FlowPane): Unit = {
    val cellWidth = 40.0
    flowPane.setStyle("-fx-border-color:red;-fx-border-width:1px;-fx-border-style:solid;")
    flowPane.setMaxWidth(9 * (cellWidth + 2) - 16)
    flowPane.setMinWidth(9 * (cellWidth + 2) - 16)
    flowPane.setPrefWrapLength(9 * (cellWidth + 2))

    val cells = 1 to 81
    val numbers = 0 to 9
    val cellz =
      for (cellCounts <- cells) yield {
        val fp = new FlowPane()
        fp.setStyle("-fx-border-color:black;-fx-border-width:1px;-fx-border-style:solid;")
        fp.setMinWidth(cellWidth)
        fp.setMinHeight(cellWidth)
        fp.setMaxWidth(cellWidth)
        fp.setMaxHeight(cellWidth)
        fp.setClip(new Rectangle(cellWidth, cellWidth))
        fp.setAlignment(Pos.CENTER)
        val icells = numbers.map(n => {
          val l = new Label(n.toString)
          l.setStyle("-fx-font-size:5px")
          l
        })
        fp.getChildren.addAll(icells)
        fp
      }
    flowPane.getChildren.addAll(cellz)
    ()
  }

  def displayResult(solution: SudokuDigitSolution, labels: Seq[Label]): Unit = {
    for ((label, s) <- labels zip solution) {
      label.setText(s.toString)
    }
  }

  def as[A](xs: Seq[_]): Seq[A] = xs.map(_.asInstanceOf[A])

  /**
   * updates UI to show for each cell (there are 81 of them) which number was detected how often
   * In each flowpane there are 9 labels (representing each number)
   *
   * @param hitCounts
   * @param displayItems
   */
  def displayHitCounts(hitCounts: HitCounts, displayItems: Seq[FlowPane]): Unit = {

    // change colors of flowpanes such that if there are more than one hits it
    // the pane should change to a orange color
    def colory(count: Int): String = {
      count match {
        case 0 => "-fx-background-color:yellow;"
        case 1 => "-fx-background-color:green;"
        case 2 => "-fx-background-color:orange;"
        case _ => "-fx-background-color:red;"
      }
    }

    for {(cellDisplay, cellContent) <- displayItems zip hitCounts
         (nDisplay, distribution) <- as[Label](cellDisplay.getChildren) zip cellContent if (distribution != 0)} {
      nDisplay.setStyle(s"-fx-font-size:${distribution}px;")
    }

    for ((cellDisplay, cellContent) <- displayItems zip hitCounts) {
      val cssColor = colory(cellContent.foldLeft(0)((acc, c) => acc + (if (c > 0) 1 else 0)))
      cellDisplay.setStyle(s"$cssColor;-fx-border-color:black;-fx-border-width:1px;-fx-border-style:solid;")
    }


  }


  def initNumberFlowPane(numberFlowPane: FlowPane) = {
    numberFlowPane.setMinWidth(3 * 142.0)
    numberFlowPane.setPrefWrapLength(3 * 142.0)
    (1 to 9).foreach(i => numberFlowPane.getChildren.add(new ImageView))
  }

  override def initialize(location: URL, resources: ResourceBundle): Unit = {

    initializeSharedState(location, resources)
    initializeCapturing(location, resources)
    // toggleButtons
    require(inputButton != null)
    require(grayedButton != null)
    require(blurredButton != null)
    require(thresholdedButton != null)
    require(invertedButton != null)
    require(dilatedButton != null)
    require(erodedButton != null)


    require(statsFlowPane != null)
    require(resultFlowPane != null)
    require(numberFlowPane != null)

    initResultPane(resultFlowPane)
    initStatsPane(statsFlowPane)
    initNumberFlowPane(numberFlowPane)
    //    modeButtons.selectedToggleProperty.addListener(mkChangeListener(onModeChange))

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
    exitCapturingMode()
    stage.close()
  }

}


