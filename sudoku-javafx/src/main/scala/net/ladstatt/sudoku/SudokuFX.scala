package net.ladstatt.sudoku

/**
 * Copyright (c) 2013-2016, Robert Ladstätter @rladstaetter
 **/

import java.io.File
import java.net.URL
import java.text.SimpleDateFormat
import java.util.concurrent.TimeUnit
import java.util.{Date, ResourceBundle}

import _root_.javafx.fxml.{FXML, Initializable}
import _root_.javafx.scene._
import _root_.javafx.scene.control._
import _root_.javafx.stage.Stage
import com.sun.javafx.perf.PerformanceTracker
import javafx.animation.FadeTransition
import javafx.application.Application
import javafx.beans.property.{SimpleBooleanProperty, SimpleIntegerProperty, SimpleObjectProperty}
import javafx.geometry.Pos
import javafx.scene.effect.{BlendMode, DropShadow}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{AnchorPane, FlowPane, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.{Circle, Polygon, Polyline, Rectangle}
import jfxtras.labs.scene.control.gauge.linear.SimpleMetroArcGauge
import net.ladstatt.core.CanLog
import net.ladstatt.opencv.OpenCV
import org.opencv.core.{Mat, MatOfPoint, Point}
import org.opencv.videoio.VideoCapture
import rx.lang.scala.{Observable, Subscription}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/**
 * For a discussion of the concepts of this application see http://ladstatt.blogspot.com/
 */
object SudokuFX {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SudokuFXApplication], args: _*)
  }
}

class SudokuFXApplication extends Application with JfxUtils {

  override def init(): Unit = {
    OpenCV.loadNativeLib()
  }

  override def start(stage: Stage): Unit =
    Try {
      stage.setTitle("SudokuFX")

      val fxmlLoader = mkFxmlLoader("/net/ladstatt/sudoku/sudokufx.fxml")
      val parent = fxmlLoader.load[VBox]()
      val controller = fxmlLoader.getController[SudokuFXController]
      val scene = new Scene(parent)
      controller.setPerformanceTracker(PerformanceTracker.getSceneTracker(scene))
      stage.setScene(scene)

      stage.setOnCloseRequest(mkEventHandler(e => {
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


class SudokuFXController extends Initializable with OpenCVJfxUtils with CanLog with JfxUtils {

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

  @FXML var frameNumberGauge: SimpleMetroArcGauge = _
  @FXML var detectionGauge: SimpleMetroArcGauge = _
  @FXML var frameRateGauge: SimpleMetroArcGauge = _
  @FXML var lastDetectionGauge: SimpleMetroArcGauge = _

  @FXML var contourModeChoiceBox: ChoiceBox[Int] = _
  @FXML var contourMethodChoiceBox: ChoiceBox[Int] = _
  @FXML var contourRatioChoiceBox: ChoiceBox[Int] = _
  @FXML var polyArea: AnchorPane = _

  val performanceTrackerProperty = new SimpleObjectProperty[PerformanceTracker]()

  def getPerformanceTracker: PerformanceTracker = performanceTrackerProperty.get()

  def setPerformanceTracker(performanceTracker: PerformanceTracker): Unit = performanceTrackerProperty.set(performanceTracker)


  val currentSudokuState = new SimpleObjectProperty[SudokuState](SudokuState.DefaultState)

  def setCurrentSudokuState(sudokuState: SudokuState): Unit = currentSudokuState.set(sudokuState)

  def getCurrentSudokuState: SudokuState = currentSudokuState.get


  val frameNumberProperty = new SimpleIntegerProperty(this, "frameNumberProperty", 0)

  def getFrameNumber: Int = frameNumberProperty.get()

  def setFrameNumber(i: Int): Unit = frameNumberProperty.set(i)


  val cameraActiveProperty = new SimpleBooleanProperty(true)

  def setCameraActive(isActive: Boolean): Unit = cameraActiveProperty.set(isActive)

  def getCameraActive: Boolean = cameraActiveProperty.get

  val basedir: File = new File("./target/runs/")
  basedir.mkdirs()
  val workingDirectory = new File(basedir, new SimpleDateFormat("yyyy-MM-dd-HH-mm").format(new Date()))
  val history: Array[File] = basedir.listFiles()


  val historyMenu: Menu = {
    val historyMenu = new Menu("Previous runs")
    val historyItems: Seq[MenuItem] = {
      history.map(f => {
        val i = new MenuItem(f.getName + s" (${f.list().length})")
        i
      })
    }
    historyMenu.getItems.addAll(historyItems.asJava)
    historyMenu
  }

  lazy val imageTemplates: Seq[Image] = TemplateLibrary.asSeq.map(toImage)

  val videoCapture = {
    val v = new VideoCapture()
    v.open(0)
    v
  }

  def aquireMat(): Mat = {
    val image: Mat = new Mat()
    if (videoCapture.isOpened) {
      videoCapture.read(image)
    } else {
      logError("videocapture closed")
    }
    image
  }


  val videoObservable: Observable[SResult] =
    Observable[Mat](o => {
      new Thread(
        new Runnable {
          override def run(): Unit = {
            while (getCameraActive) {
              Try {
                val m = aquireMat()
                //  Imgcodecs.imwrite(Paths.get("/Users/lad/Documents/sudokufx/target").resolve(UUID.randomUUID() + ".png").toAbsolutePath.toString, m)
                m
              } match {
                case Success(m) => o.onNext(m)
                case Failure(e) => o.onError(e)
              }
            }
            logInfo("Shutting down video service ... ")
            videoCapture.release()
            o.onCompleted()
          }
        }).start()
      new Subscription {}
    }).zipWithIndex.map {
      case (frame, index) =>
        val params: SParams = SParams(contourModeChoiceBox.getValue, contourMethodChoiceBox.getValue, contourRatioChoiceBox.getValue)
        val pipeline: FramePipeline = FramePipeline(frame, params)
        pipeline.detectRectangle match {
          case None => pipeline
          case Some(rect) => SCandidate(index, pipeline, SRectangle(pipeline.frame, rect, pipeline.corners), getCurrentSudokuState)
        }
    }.delaySubscription(Duration(2000, TimeUnit.MILLISECONDS))


  def displayContours(contours: Seq[MatOfPoint]): Future[Unit] = {
    val polys =
      contours.map {
        c =>
          val p = new Polygon()
          c.toList.asScala.foldLeft(p)((acc, p) => {
            acc.getPoints.addAll(p.x, p.y)
            acc
          })
          p.setStroke(Color.AQUAMARINE)
          p
      }
    execOnUIThread({
      polyArea.getChildren.clear()
      polyArea.getChildren.addAll(polys.asJava)
    })
  }

  def display(f: FramePipeline): Unit = {
    setVideoView(f.frame)
  }

  def process(result: SResult, state: SudokuState): Unit = {
    result match {
      case f: FramePipeline =>
        display(f)
      case c: SCandidate =>
        for {
          (result, nextState) <- c.calc
        } {
          setCurrentSudokuState(nextState)
          // displayContours(candidate.contours)
          display(result)
        }
    }
  }

  def resetState(): Unit = setCurrentSudokuState(SudokuState.DefaultState)

  def shutdown(): Unit = {
    setCameraActive(false)
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
    val imageViews = imageTemplates.map(new ImageView(_))
    templateToolBar.getItems.addAll(imageViews.asJava)
    canvas.getChildren.add(sudokuBorder)
    canvas.getChildren.addAll(analysisCellBounds.toList.asJava)
    canvas.getChildren.addAll(analysisCellCorners.toList.asJava)
    frameNumberGauge.setMaxValue(5000)
    frameNumberGauge.setMinValue(0)
    detectionGauge.setMinValue(0)
    detectionGauge.setMaxValue(500)
    frameRateGauge.setMinValue(0)
    frameRateGauge.setMaxValue(60)
    lastDetectionGauge.setMinValue(0)
    lastDetectionGauge.setMaxValue(81)

    ()
  }


  def updateStatus(message: String, color: Color): Unit = {
    statusLabel.setTextFill(color)
    statusLabel.setText(message)
  }

  def updateDigitLibraryView(digitLibrary: DigitLibrary, nrViews: Seq[ImageView]): Future[Unit] = {
    // show recognized digits
    execOnUIThread(
      for (i <- Parameters.range) {
        digitLibrary(i + 1)._2.foreach(m => nrViews(i).setImage(toImage(m)))
      })
  }


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
    line.setOnMouseEntered(mkEventHandler(e => {
      analysisFadeIn.play()
      logInfo(s"entered box $idx")
    }))
    line.setOnMouseExited(mkEventHandler(e => {
      analysisFadeOut.play()
      logInfo(s"left box $idx")
    }))
    line
  }

  val sudokuBorder: Polyline = {
    val line = new Polyline()
    line.setStroke(Color.BLUE)
    line.setStrokeWidth(5)
    line.setEffect(new DropShadow())
    line.setOnMouseEntered(mkEventHandler(e => {
      line.setOpacity(1.0)
    }))
    line.setOnMouseExited(mkEventHandler(e => {
      borderFadeTransition.play()
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


  workingDirectory.mkdirs()

  /**
   * where application will put captured frames
   */
  val workingDirectoryProperty = new SimpleObjectProperty[File](workingDirectory)

  def getWorkingDirectory: File = workingDirectoryProperty.get

  def setWorkingDirectory(file: File): Unit = workingDirectoryProperty.set(file)

  def setVideoView(mat: Mat): Unit = {
    val image: Image = toImage(mat)
    videoView.setImage(image)
  }


  def mkRange(a: Double, b: Double, nrCells: Int = 9): Seq[Double] = {
    if (a == b) {
      List.fill(10)(a)
    } else {
      val r: Seq[BigDecimal] = BigDecimal(a) to BigDecimal(b) by BigDecimal((b - a) / nrCells)
      if (r.size == 9) {
        List.concat(r.map(_.toDouble), List(b))
      } else {
        r.map(_.toDouble)
      }
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
    c
  }


  /**
   * returns coordinates of the 100 cell corners
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
    for (i <- 0 to 88 if !exclusions.contains(i)) yield extractBound(i)
  }

  def updateCellBounds(border: List[Point], cellBounds: Array[Polyline]): Unit = {
    val cellCorners = mkCellCorners(border)
    if (cellCorners.size == 100) {
      val boundCoordinates = mkCellBounds(cellCorners)
      if (boundCoordinates.size == 81) {
        for (i <- 0 to 80) {
          val line = cellBounds(i)
          line.getPoints.clear()
          line.getPoints.addAll(boundCoordinates(i).asJava)
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
      case ((x, y), index) =>
        cellCorners(index).setCenterX(x)
        cellCorners(index).setCenterY(y)
        mkFadeTransition(500, cellCorners(index), 1.0, 0.0).play()
    }
  }

  def updateBorder(corners: List[Point]): Unit = {
    sudokuBorder.getPoints.clear()
    sudokuBorder.getPoints.addAll(convert2PolyLinePoints(corners).asJava)
    borderFadeTransition.play()
  }

  def updateDisplay(stage: ProcessingStage, sudokuResult: SudokuResult): Unit = {

    def updateVideo(stage: ProcessingStage, imageIoChain: FramePipeline, solutionMat: Mat): Unit = {
      stage match {
        case InputStage => setVideoView(imageIoChain.frame)
        case GrayedStage => setVideoView(imageIoChain.grayed)
        case BlurredStage => setVideoView(imageIoChain.blurred)
        case ThresholdedStage => setVideoView(imageIoChain.thresholded)
        case InvertedStage => setVideoView(imageIoChain.inverted)
        case DilatedStage => setVideoView(imageIoChain.dilated)
        case ErodedStage => setVideoView(imageIoChain.eroded)
        case SolutionStage => setVideoView(solutionMat)
      }
    }

    displayHitCounts(getCurrentSudokuState.hitCounts, as[FlowPane](statsFlowPane.getChildren.asScala.toSeq))

    sudokuResult match {
      case SSuccess(SCandidate(nr, framePipeline, sr, ss), SRectangle(sudokuCanvas, detectedCells, corners), someSolution) =>
        if (someSolution.isDefined) {
          val sol = someSolution.get
          updateVideo(stage, framePipeline, sol.solutionMat)
          displayResult(sol.solution, as[Label](resultFlowPane.getChildren.asScala.toSeq))
        } else {
          updateVideo(stage, framePipeline, framePipeline.frame)
        }
      case SFailure(msg, SCandidate(nr, framePipeline, sr, ss)) =>
        updateVideo(stage, framePipeline, framePipeline.frame)
    }

  }

  def display(result: SudokuResult): Future[Unit] = execOnUIThread {
    val selectedToggle = viewButtons.getSelectedToggle
    if (selectedToggle != null) {
      val userData = selectedToggle.getUserData
      if (userData != null) {
        updateDisplay(userData.asInstanceOf[ProcessingStage], result)
      }
    }


    setAnalysisMouseTransparent(false)
    updateDigitLibraryView(getCurrentSudokuState.library, as[ImageView](numberFlowPane.getChildren.asScala.toSeq))

    frameRateGauge.setValue(Float.float2double(getPerformanceTracker.getAverageFPS))
    result match {
      case success: SSuccess if success.someSolution.isDefined =>
        detectionGauge.setValue(detectionGauge.getValue + 1)
        lastDetectionGauge.setValue(Int.int2double(success.sudokuFrame.detectedCells.length))
        updateStatus(mkFps(success.inputFrame.pipeline.start), Color.GREEN)
      case onlyCorners: SSuccess if onlyCorners.someSolution.isEmpty =>
        updateStatus(mkFps(onlyCorners.inputFrame.pipeline.start), Color.ORANGE)
        updateCellBounds(onlyCorners.sudokuFrame.corners, analysisCellBounds)
        updateCellCorners(onlyCorners.sudokuFrame.corners, analysisCellCorners)
      case SFailure(msg, SCandidate(_, framePipeline, sr, SudokuState.DefaultState)) => updateStatus(mkFps(framePipeline.start), Color.AQUA)
    }
  }


  def mkFps(start: Long): String = {
    def mkDuration = {
      val after = System.nanoTime
      (after - start) / 1000000
    }

    s"FPS " + f"${getPerformanceTracker.getAverageFPS}%3.2f" + s" Frame: $mkDuration ms"
  }

  def showAbout(): Unit = {
    /*
    val d = new Alert(AlertType.INFORMATION)
    d.setTitle("About SudokuFX")
    d.setHeaderText("SudokuFX (c) @rladstaetter 2013 - 2015")
    d.setContentText("Use this application to solve Sudokus.")
    d.showAndWait()
    ()
    */
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
    resultPane.getChildren.addAll(mkSolutionPane().asJava)
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
        fp.getChildren.addAll(icells.asJava)
        fp
      }
    flowPane.getChildren.addAll(cellz.asJava)
    ()
  }

  def displayResult(solution: SudokuDigitSolution, labels: Seq[Label]): Unit = {
    for ((label, s) <- labels zip solution) {
      label.setText(s.toString)
    }
  }


  /**
   * updates UI to show for each cell (there are 81 of them) which number was detected how often
   * In each flowpane there are 9 labels (representing each number)
   */
  def displayHitCounts(hitCounts: HitCounters, displayItems: Seq[FlowPane]): Unit = {

    // change colors of flowpanes such that if there are more than one hits it
    // the pane should change to a orange color
    def colory(count: Int): String = {
      count match {
        case 1 => "-fx-background-color:green;"
        case 2 => "-fx-background-color:orange;"
        case _ => "-fx-background-color:red;"
      }
    }

    val sortedHitCountValues = hitCounts.toSeq.sortWith { case (a, b) => a._1 < b._1 }.map(_._2)

    for {(cellDisplay, cellContent) <- displayItems zip sortedHitCountValues
         (v, distribution) <- cellContent.toSeq} {
      val fontSize = if (distribution < Parameters.topCap) distribution else Parameters.topCap
      cellDisplay.getChildren.get(v).setStyle(s"-fx-font-size:${fontSize}px;")
    }

    // if there are ambiguities, display them in red
    for ((cellDisplay, cellContent) <- displayItems zip sortedHitCountValues) {
      val cssColor = colory(cellContent.size)
      cellDisplay.setStyle(s"$cssColor;-fx-border-color:black;-fx-border-width:1px;-fx-border-style:solid;")
    }


  }


  def initNumberFlowPane(numberFlowPane: FlowPane): Unit = {
    numberFlowPane.setMinWidth(3 * 142.0)
    numberFlowPane.setPrefWrapLength(3 * 142.0)
    (1 to 9).foreach(i => numberFlowPane.getChildren.add(new ImageView))
  }

  def initializeChoiceBoxes(): Unit = {
    contourModeChoiceBox.getItems.addAll(0, 1, 2, 3)
    contourMethodChoiceBox.getItems.addAll(1, 2, 3, 4)
    contourRatioChoiceBox.getItems.addAll((0 to 60 by 5).asJava)
    contourModeChoiceBox.setValue(3)
    contourMethodChoiceBox.setValue(2)
    contourRatioChoiceBox.setValue(30)
  }


  override def initialize(location: URL, resources: ResourceBundle): Unit = {

    initializeChoiceBoxes()
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


    // startCapture
    videoObservable.subscribe((result: SResult) => process(result, getCurrentSudokuState),
      t => t.printStackTrace(),
      () => logInfo("Videostream stopped..."))
    ()
  }


}

