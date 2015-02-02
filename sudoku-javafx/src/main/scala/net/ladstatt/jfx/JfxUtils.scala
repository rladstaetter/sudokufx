package net.ladstatt.jfx

import java.awt.image.BufferedImage
import java.io.File
import java.util.{Timer, TimerTask}
import javafx.application.Platform
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.{FXCollections, ListChangeListener, ObservableList}
import javafx.concurrent.Task
import javafx.embed.swing.SwingFXUtils
import javafx.event.{Event, EventHandler}
import javafx.fxml.{FXMLLoader, JavaFXBuilderFactory}
import javafx.geometry.Orientation
import javafx.scene.control._
import javafx.scene.image.Image
import javafx.util.Callback

import net.ladstatt.core.{FutureUtils, Utils}
import org.opencv.core.{Mat, Point}
import org.opencv.highgui.{Highgui, VideoCapture}

import scala.collection.JavaConversions._
import scala.concurrent.{ExecutionContext, Future}


case class FrameGrabberTask(processFrame: (ObservableValue[_ <: Mat], Mat, Mat) => Unit) extends TimerTask
with JfxUtils with Utils {

  val mainLoop: ChangeListener[Mat] = mkChangeListener(processFrame)
  val videoCapture = new VideoCapture(0)

  lazy val sudokuProperty = {
    val sop = new SimpleObjectProperty[Mat]
    sop.addListener(mainLoop)
    sop
  }

  override def cancel(): Boolean = {
    super.cancel
    sudokuProperty.removeListener(mainLoop)
    true
  }

  override def run(): Unit = {
    sudokuProperty.set(aquireMat)
  }


  //  videoCapture.set(Highgui.CV_CAP_PROP_FRAME_WIDTH,800)
  //  videoCapture.set(Highgui.CV_CAP_PROP_FRAME_HEIGHT,600)


  def aquireMat() = {
    val image = new Mat()
    if (videoCapture.isOpened) {
      videoCapture.read(image)
      isNull(image)(throw new RuntimeException("videoCapture.read(...) returned null"), image => image)
    } else {
      throw new RuntimeException("Video capture device is closed.")
    }
  }

}

class FrameTimer extends Timer {

  def schedule(task: FrameGrabberTask, delay: Long = 0, period: Long = 1) = {
    super.schedule(task, delay, period)
  }
}

trait OpenCVJfxUtils extends Utils {

  /**
   * runtime:
   *
   * Benchmark                                  Mode   Samples         Mean   Mean error    Units
   * n.l.a.s.SudokuBenchmark.measureToImage     avgt        10       17.138        0.386    ms/op
   * n.l.a.s.SudokuBenchmark.measureToImage     avgt        10       17.084        0.314    ms/op
   * n.l.a.s.SudokuBenchmark.measureToImage     avgt        10       16.232        0.250    ms/op  (while loops)
   * @param matrix
   * @return
   */
  def toImage(matrix: Mat): Image = {
    val cols = matrix.cols()
    val rows = matrix.rows()
    val elemSize = matrix.elemSize()
    val data: Array[Byte] = new Array[Byte](cols * rows * elemSize.toInt)
    matrix.get(0, 0, data)

    val lType = matrix.channels() match {
      case 1 => BufferedImage.TYPE_BYTE_GRAY
      case 3 => BufferedImage.TYPE_3BYTE_BGR
      case 4 => BufferedImage.TYPE_4BYTE_ABGR
      case _ => {
        BufferedImage.TYPE_BYTE_GRAY
      }
    }

    matrix.channels() match {
      case 3 => {
        var i = 0
        while (i < data.length) {
          val b = data(i)
          data(i) = data(i + 2)
          data(i + 2) = b
          i = i + 3
        }
      }
      case 4 => {
        var i = 0
        while (i < data.length) {
          val b = data(i)
          data(i) = data(i + 2)
          data(i + 2) = b
          i = i + 4
        }
      }
      case _ => {}
    }

    val image = new BufferedImage(cols, rows, lType)
    image.getRaster().setDataElements(0, 0, cols, rows, data)
    SwingFXUtils.toFXImage(image, null)
  }


  def convert2PolyLinePoints(points: Iterable[Point]): List[java.lang.Double] = {
    if (points.isEmpty)
      List()
    else {
      val ps = points.map(p => List[java.lang.Double](p.x, p.y)).flatten.toList
      ps ++ List(ps(0), ps(1))
    }
  }

  def loadImage(implicit ec: ExecutionContext, file: File): Future[Mat] =
    FutureUtils.execFuture {
      Highgui.imread(file.getAbsolutePath)
    }

}

trait JfxUtils {

  def mkObservableList[T](iterable: Iterable[T]): ObservableList[T] = {
    val mutableList = new java.util.ArrayList[T]
    mutableList.addAll(iterable)
    FXCollections.observableList(mutableList)
  }

  def execOnUIThread(f: => Unit) {
    Platform.runLater(new Runnable {
      override def run() = f
    })
  }

  /**
   * utility function to create slider instances.
   */
  def mkSlider(min: Double, max: Double, initialValue: Double, orientation: Orientation): Slider = {
    require(min <= initialValue)
    require(initialValue <= max)
    val slider = new Slider()
    slider.setMin(min)
    slider.setMax(max)
    slider.setValue(initialValue)
    slider.setShowTickLabels(true)
    slider.setShowTickMarks(true)
    slider.setBlockIncrement(1)
    slider.setOrientation(orientation)
    slider
  }

  def mkChangeListener[T](onChangeAction: (ObservableValue[_ <: T], T, T) => Unit): ChangeListener[T] = {
    new ChangeListener[T]() {
      override def changed(observable: ObservableValue[_ <: T], oldValue: T, newValue: T) = {
        onChangeAction(observable, oldValue, newValue)
      }
    }
  }

  def mkListChangeListener[E](onChangedAction: ListChangeListener.Change[_ <: E] => Unit) = new ListChangeListener[E] {
    def onChanged(changeItem: ListChangeListener.Change[_ <: E]): Unit = {
      onChangedAction(changeItem)
    }
  }

  def mkCellFactoryCallback[T](listCellGenerator: ListView[T] => ListCell[T]) = new Callback[ListView[T], ListCell[T]]() {
    override def call(list: ListView[T]): ListCell[T] = listCellGenerator(list)
  }

  def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] {
    def handle(e: E) = f(e)
  }

  def mkFxmlLoader(cpResource: String) = {
    val location = getClass.getResource(cpResource)
    require(location != null, s"Could not resolve $cpResource: Location was null.")
    val fxmlLoader = new FXMLLoader()
    fxmlLoader.setLocation(location)
    fxmlLoader.setBuilderFactory(new JavaFXBuilderFactory())
    fxmlLoader
  }

  def mkFxmlLoader(fxmlResource: String, controller: AnyRef): FXMLLoader = {
    val loader = mkFxmlLoader(fxmlResource)
    loader.setController(controller)
    loader
  }

  def mk[A](fxmlLoader: FXMLLoader): A = {
    fxmlLoader.load()
  }

  def mkTask[X](callFn: => X): Task[X] = new Task[X] {
    override def call(): X = callFn
  }

}

