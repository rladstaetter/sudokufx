package net.ladstatt.jfx

import javafx.geometry.Orientation
import javafx.scene.control._
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.{FXCollections, ObservableList, ListChangeListener}
import javafx.util.{StringConverter, Callback}
import javafx.event.{EventHandler, Event}
import javafx.concurrent.Task
import javafx.fxml.{JavaFXBuilderFactory, FXMLLoader}
import org.opencv.core.{Point, Mat}
import javafx.scene.image.Image
import java.awt.image.{ImageProducer, BufferedImage}
import javafx.embed.swing.SwingFXUtils
import javafx.scene.shape.Polyline
import javafx.scene.paint.Color
import javafx.scene.effect.DropShadow
import scala.collection.JavaConversions._
import javafx.beans.property.{SimpleBooleanProperty, SimpleObjectProperty}
import java.util.{TimerTask, Timer}
import javafx.application.Platform
import scala.util.{Random, Try}
import org.opencv.highgui.{Highgui, VideoCapture}
import net.ladstatt.core.{HasDescription, Utils}
import java.io.{InputStream, FilenameFilter, File}
import scala.concurrent.{ExecutionContext, Future, Await, Promise}
import scala.concurrent.duration.Duration


class StringConverter4HasDescription[T <: HasDescription] extends StringConverter[T] {
  override def toString(t: T): String = t.description

  override def fromString(string: String): T = ???
}


trait FrameGrabberTask {
  def mainLoop: ChangeListener[Mat]

  lazy val frameProperty = {
    val sop = new SimpleObjectProperty[Mat]
    sop.addListener(mainLoop)
    sop
  }

}

/**
 * defines a task which is run periodically
 */
trait TimedFrameGrabberTask extends TimerTask
with FrameGrabberTask
with Utils {

  override def cancel = {
    super.cancel
    frameProperty.removeListener(mainLoop)
    true
  }

  def aquireMat(): Mat

  override def run(): Unit = {
    frameProperty.set(aquireMat)
  }


}

/*
case class FileGrabberTimedTask(dir: File, val mainLoop: ChangeListener[Try[Mat]]) extends TimedFrameGrabberTask {
val files =
dir.listFiles(new FilenameFilter {
override def accept(dir: File, name: String): Boolean = {
name.endsWith(".png") || name.endsWith(".gif") || name.endsWith(".jpg") || name.endsWith(".jpeg")
}
})

var cnt = 1

override def aquireMat: Try[Mat] = Try {
if (files.size == 0) throw new RuntimeException(s"Could not find any sudoku image in ${dir.getAbsolutePath}")
val image = Highgui.imread(files(cnt).getAbsolutePath)
cnt = cnt + 1
if (cnt == files.size) cnt = 1
image
}

}
*/
/*
case class MatSeqGrabberTimed(mats : Seq[Mat], val mainLoop: ChangeListener[Try[Mat]]) extends TimedFrameGrabberTask {


var pos = 0

override def aquireMat: Try[Mat] = Try {
  val mat = mats(pos)
  if (pos < mats.size - 1)
    pos = pos + 1
  else
    pos = 0
  mat
}

}
       */
/*
case class HistoryFrameGrabberTask(val mainLoop: ChangeListener[Mat]) extends TimedFrameGrabberTask {
override def aquireMat: Try[Mat] = ???

override def run(): Unit = ()

override def cancel: Boolean = {
frameProperty.unbind
true
}
}
*/
case class OpenCVTimedFrameGrabberTask(videoCapture: VideoCapture,
                                       mainLoop: ChangeListener[Mat]) extends TimedFrameGrabberTask {

//  videoCapture.set(Highgui.CV_CAP_PROP_FRAME_WIDTH,800)
//  videoCapture.set(Highgui.CV_CAP_PROP_FRAME_HEIGHT,600)

  override def cancel(): Boolean = {
    super.cancel
    // kills jvm and is the reason why the camera always runs
    //    videoCapture.release
    true
  }

  override def aquireMat = {
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

  def schedule(task: TimedFrameGrabberTask, delay: Long = 0, period: Long = 1) = {
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
        //println(matrix.channels())
        BufferedImage.TYPE_BYTE_GRAY
      }
    }

    // todo improve swapping of data !
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
      case _ => {
        // do nothing?
        // println("?" + matrix.channels())
      }
    }

    val image = new BufferedImage(cols, rows, lType)
    image.getRaster().setDataElements(0, 0, cols, rows, data)
    SwingFXUtils.toFXImage(image, null)
  }


  def convert2PolyLinePoints(points: Iterable[Point]): List[java.lang.Double] = {
    val ps = points.map(p => List[java.lang.Double](p.x, p.y)).flatten.toList
    ps ++ List(ps(0), ps(1))
  }

  /*
 @deprecated("use mkPolyLine() and convert2PolyLinePoints instead","latest")
 def mkPolyLine(points: Iterable[Point]): Polyline = {
   val line = mkPolyLine()
   line.getPoints.addAll(convert2PolyLinePoints(points))
   line
 }
 */

  def loadImage(implicit ec: ExecutionContext, file: File): Future[Mat] =
    execFuture {
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

