package net.ladstatt.apps.sudoku

import java.io.File
import java.net.URL
import java.text.SimpleDateFormat
import java.util.{Date, ResourceBundle}
import javafx.animation.FadeTransition
import javafx.beans.property.SimpleObjectProperty
import javafx.fxml.FXML
import javafx.scene.Node
import javafx.scene.control._
import javafx.scene.effect.{BlendMode, DropShadow}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.AnchorPane
import javafx.scene.paint.Color
import javafx.scene.shape.{Circle, Polyline, Rectangle}

import com.sun.javafx.perf.PerformanceTracker
import net.ladstatt.core.CanLog
import net.ladstatt.jfx.{JfxUtils, OpenCVJfxUtils}
import org.opencv.core.{Mat, MatOfPoint2f}

import scala.collection.JavaConversions._

/**
 * Created by lad on 31.12.14.
 */
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
  @FXML var historyToolBar: ToolBar = _
  @FXML var mainMenuBar: MenuBar = _

  lazy val nrViews: Array[ImageView] =
    Array(nrView1, nrView2, nrView3,
      nrView4, nrView5, nrView6,
      nrView7, nrView8, nrView9)

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

  lazy val imageTemplates: Map[Int, Image] = Parameters.templateLibrary.map { case (i, m) => i -> toImage(m)}

  def initializeSharedState(location: URL, resources: ResourceBundle): Unit = {
    require(historyToolBar != null)
    require(statusLabel != null)
    require(bestMatchToolBar != null)

    require(templateToolBar != null)
    require(mainMenuBar != null)

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

  def updateBestMatch(sudokuHistory: SCandidate): Unit = {
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

  def display(result: SudokuResult) = execOnUIThread {
    result match {
      case success: SSuccess => {
        updateDisplay(viewButtons.getSelectedToggle.getUserData.asInstanceOf[ProcessingStage], result)
        setAnalysisMouseTransparent(false)
        updateBestMatch(success.candidate)
        updateStatus(mkFps(success.candidate.start), Color.GREEN)
        updateBorder(success.candidate.sudokuCorners)
        updateCellBounds(success.candidate.sudokuCorners, analysisCellBounds)
        updateCellCorners(success.candidate.sudokuCorners, analysisCellCorners)
      }
      case SFailure(candidate) => {
        updateDisplay(viewButtons.getSelectedToggle.getUserData.asInstanceOf[ProcessingStage], result)
        setAnalysisMouseTransparent(false)
        updateBestMatch(candidate)
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

}
