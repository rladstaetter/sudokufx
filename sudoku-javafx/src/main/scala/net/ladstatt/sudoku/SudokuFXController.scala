package net.ladstatt.sudoku

import java.io.File
import java.net.URL
import java.text.SimpleDateFormat
import java.util.concurrent.TimeUnit
import java.util.{Date, ResourceBundle}

import _root_.javafx.fxml.{FXML, Initializable}
import _root_.javafx.scene._
import _root_.javafx.scene.control._
import com.sun.javafx.perf.PerformanceTracker
import javafx.animation.FadeTransition
import javafx.beans.property.{SimpleBooleanProperty, SimpleIntegerProperty, SimpleObjectProperty}
import javafx.geometry.Pos
import javafx.scene.effect.{BlendMode, DropShadow}
import javafx.scene.image._
import javafx.scene.layout.{AnchorPane, FlowPane}
import javafx.scene.paint.Color
import javafx.scene.shape.{Circle, Polyline, Rectangle}
import net.ladstatt.core.CanLog
import org.bytedeco.javacv.{Frame, OpenCVFrameConverter, OpenCVFrameGrabber}
import org.bytedeco.opencv.opencv_core.Mat
import rx.lang.scala.Observable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}


class SudokuFXController extends Initializable with OpenCVJfxUtils with CanLog with JfxUtils {

  val javaCVConv = new OpenCVFrameConverter.ToMat


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
      }).toIndexedSeq
    }
    historyMenu.getItems.addAll(historyItems.asJava)
    historyMenu
  }

  // lazy val imageTemplates: Seq[Image] = TemplateLibrary.asSeq.map(JavaCVPainter.toImage)

  val frameGrabber: OpenCVFrameGrabber = {
    val grabber = new OpenCVFrameGrabber(0)
    grabber.start()
    grabber
  }

  val videoObservable: Observable[SResult] =
    Observable[Frame](o => {
      new Thread(
        () => {
          while (getCameraActive) {
            Try {
              frameGrabber.grab()
            } match {
              case Success(m) => o.onNext(m)
              case Failure(e) =>
                e.printStackTrace()
                o.onError(e)
            }
          }
          logInfo("Shutting down video service ... ")
          frameGrabber.release()
          o.onCompleted()
        }).start()
    }).zipWithIndex.map {
      case (frame, index) =>
        val params: SParams = SParams(contourModeChoiceBox.getValue, contourMethodChoiceBox.getValue, contourRatioChoiceBox.getValue)
        val pipeline: FramePipeline = FramePipeline(frame, params)
        /*
        pipeline.detectRectangle match {
          case None => pipeline
          case Some(rect) => SCandidate(index, pipeline, SRectangle(pipeline.frame, rect, pipeline.corners), getCurrentSudokuState)
        } */
        pipeline
    }.delaySubscription(Duration(2000, TimeUnit.MILLISECONDS))

  /*
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
        ()
      })
    }
  */
  def display(framePipeline: FramePipeline): Unit = setVideoView(framePipeline.frame)

  def onResult(result: SResult): Unit = {
    result match {
      case f: FramePipeline => updateVideo(f, f.frame)
      case c: SCandidate =>
        for {
          (result, nextState) <- c.calc
        } {
          setCurrentSudokuState(nextState)
          updateDisplay(result)
        }
    }
  }

  def resetState(): Unit = setCurrentSudokuState(SudokuState.DefaultState)

  def shutdown(): Unit = {
    setCameraActive(false)
  }

  def initializeCapturing(): Unit = {
    require(Option(viewButtons).isDefined)
    require(Option(videoView).isDefined)
    require(Option(solutionButton).isDefined)
  }

  def initializeSharedState(): Unit = {
    require(Option(statusLabel).isDefined)
    require(Option(templateToolBar).isDefined)
    require(Option(mainMenuBar).isDefined)

    mainMenuBar.getMenus.add(historyMenu)
    // val imageViews = imageTemplates.map(new ImageView(_))
    // templateToolBar.getItems.addAll(imageViews.asJava)
    canvas.getChildren.add(sudokuBorder)
    canvas.getChildren.addAll(analysisCellBounds.toList.asJava)
    canvas.getChildren.addAll(analysisCellCorners.toList.asJava)

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
        digitLibrary(i + 1)._2.foreach(m => nrViews(i).setImage(JavaCVPainter.toImage(m)))
      })
  }


  val analysisCellCorners: Array[Circle] = Array.tabulate(100)(_ => mkCellCorner)
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
    line.setOnMouseEntered(mkEventHandler(_ => {
      analysisFadeIn.play()
      logInfo(s"entered box $idx")
    }))
    line.setOnMouseExited(mkEventHandler(_ => {
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
    line.setOnMouseEntered(mkEventHandler(_ => {
      line.setOpacity(1.0)
    }))
    line.setOnMouseExited(mkEventHandler(_ => {
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
    videoView.setImage(JavaCVPainter.toImage(mat))
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

  def mkCellCorner: Circle = {
    val c = new Circle
    c.setRadius(3)
    c.setStroke(Color.GOLD)
    c.setFill(Color.INDIANRED)
    c
  }


  /**
   * returns coordinates of the 100 cell corners
   */
  /*
def mkCellCorners(corners: Seq[Point]): Seq[(Double, Double)] = {
  val Seq(ul, ur, lr, ll) = corners
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
} */

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

  /*
    def updateCellBounds(border: Seq[Point], cellBounds: Array[Polyline]): Unit = {
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

    def updateCellCorners(corners: Seq[Point], cellCorners: Array[Circle]): Unit = {
      mkCellCorners(corners).zipWithIndex.foreach {
        case ((x, y), index) =>
          cellCorners(index).setCenterX(x)
          cellCorners(index).setCenterY(y)
          mkFadeTransition(500, cellCorners(index), 1.0, 0.0).play()
      }
    }
    def updateBorder(corners: List[Point]): Unit = {
      sudokuBorder.getPoints.clear()
      // sudokuBorder.getPoints.addAll(convert2PolyLinePoints(corners).asJava)
      // borderFadeTransition.play()
    }
  */

  def updateVideo(fp: FramePipeline, solutionMat: Mat): Unit = {
    for {
      selectedToggle <- Option(viewButtons.getSelectedToggle)
      processingStage <- Option(selectedToggle.getUserData).map(_.asInstanceOf[ProcessingStage])
    } yield {
      println(processingStage)
      processingStage match {
        case InputStage => setVideoView(fp.frame)
        case GrayedStage => setVideoView(fp.grayed)
        case BlurredStage => setVideoView(fp.blurred)
        case ThresholdedStage => setVideoView(fp.thresholded)
        case InvertedStage => setVideoView(fp.inverted)
        case DilatedStage => setVideoView(fp.dilated)
        case ErodedStage => setVideoView(fp.eroded)
        case SolutionStage => setVideoView(solutionMat)
      }
    }
  }

  def updateDisplay(sudokuResult: SudokuResult): Unit = {
    // displayHitCounts(getCurrentSudokuState.hitCounts, as[FlowPane](statsFlowPane.getChildren.asScala.toSeq))

    sudokuResult match {
      case SSuccess(sc: SCandidate, _, someSolution) =>
        if (someSolution.isDefined) {
          val sol = someSolution.get
          updateVideo(sc.pipeline, sol.solutionMat)
          displayResult(sol.solution, as[Label](resultFlowPane.getChildren.asScala.toSeq))
        } else {
          updateVideo(sc.pipeline, sc.pipeline.frame)
        }
      case SFailure(_, sc: SCandidate) =>
        updateVideo(sc.pipeline, sc.pipeline.frame)
    }

  }

  /*
def display(result: SudokuResult): Future[Unit] = execOnUIThread {
  for {
    selectedToggle <- Option(viewButtons.getSelectedToggle)
    processingStage <- Option(selectedToggle.getUserData).map(_.asInstanceOf[ProcessingStage])
  } yield {
    updateDisplay(result)
  }

      setAnalysisMouseTransparent(false)

      // updateDigitLibraryView(getCurrentSudokuState.library, as[ImageView](numberFlowPane.getChildren.asScala.toSeq))
      result match {
        case success: SSuccess if success.someSolution.isDefined =>
          updateStatus(mkFps(success.inputFrame.pipeline.start), Color.GREEN)
        case onlyCorners: SSuccess if onlyCorners.someSolution.isEmpty =>
          updateStatus(mkFps(onlyCorners.inputFrame.pipeline.start), Color.ORANGE)
        // updateCellBounds(onlyCorners.sudokuFrame.corners, analysisCellBounds)
        // updateCellCorners(onlyCorners.sudokuFrame.corners, analysisCellCorners)
        case SFailure(_, SCandidate(_, framePipeline, _, SudokuState.DefaultState)) => updateStatus(mkFps(framePipeline.start), Color.AQUA)
      }

}
   */


  def mkFps(start: Long): String = {
    def mkDuration: Long = {
      val after = System.nanoTime
      (after - start) / 1000000
    }

    s"FPS " + f"${
      getPerformanceTracker.getAverageFPS
    }%3.2f" + s" Frame: $mkDuration ms"
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
      for (_ <- cells) yield {
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
      for (_ <- cells) yield {
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

    val sortedHitCountValues = hitCounts.toSeq.sortWith {
      case (a, b) => a._1 < b._1
    }.map(_._2)

    for {
      (cellDisplay, cellContent) <- displayItems zip sortedHitCountValues
      (v, distribution) <- cellContent.toSeq
    } {
      val fontSize = if (distribution < Parameters.topCap) distribution else Parameters.topCap
      cellDisplay.getChildren.get(v).setStyle(s"-fx-font-size:${
        fontSize
      }px;")
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
    (1 to 9).foreach(_ => numberFlowPane.getChildren.add(new ImageView))
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
    initializeSharedState()
    initializeCapturing()

    // toggleButtons
    val toggleButtonsMap =
      Map[ToggleButton, ProcessingStage](
        inputButton -> InputStage
        , grayedButton -> GrayedStage
        , blurredButton -> BlurredStage
        , thresholdedButton -> ThresholdedStage
        , invertedButton -> InvertedStage
        , dilatedButton -> DilatedStage
        , erodedButton -> ErodedStage)

    require(toggleButtonsMap.keySet.forall(b => Option(b).isDefined))
    toggleButtonsMap.foreach {
      case (tb, stage) => tb.setUserData(stage)
    }

    val flowPanes = Set[FlowPane](statsFlowPane, resultFlowPane, numberFlowPane)
    require(flowPanes.forall(fp => Option(fp).isDefined))


    initResultPane(resultFlowPane)
    initStatsPane(statsFlowPane)
    initNumberFlowPane(numberFlowPane)
    //    modeButtons.selectedToggleProperty.addListener(mkChangeListener(onModeChange))

    // startCapture
    videoObservable.subscribe(
      onResult,
      t => t.printStackTrace(),
      () => logInfo("Videostream stopped..."))
    ()
  }

}
