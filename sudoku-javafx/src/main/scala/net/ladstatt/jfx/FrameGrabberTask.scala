package net.ladstatt.jfx

import java.util.TimerTask
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.value.{ChangeListener, ObservableValue}

import net.ladstatt.core.Utils
import org.opencv.core.Mat
import org.opencv.highgui.VideoCapture

/**
 * Created by lad on 09.02.15.
 */
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
    sudokuProperty.set(aquireMat())
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
