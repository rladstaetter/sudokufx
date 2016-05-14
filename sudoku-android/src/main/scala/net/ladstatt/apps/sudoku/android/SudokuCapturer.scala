package net.ladstatt.apps.sudoku.android

import _root_.android.app.Activity
import _root_.android.os.{Bundle, Handler, Process}
import _root_.android.util.Log
import _root_.android.view.View.OnClickListener
import _root_.android.view.{Gravity, View, WindowManager}
import _root_.android.widget.{Button, FrameLayout}
import com.google.ads.{AdRequest, AdSize, AdView}
import net.ladstatt.sudoku._
import org.opencv.android.CameraBridgeViewBase.CvCameraViewListener2
import org.opencv.android.{CameraBridgeViewBase, OpenCVLoader}
import org.opencv.core.Mat

import scala.concurrent.Await
import scala.concurrent.duration._


trait CanLog {

  val TAG = "SudokuCapturer"

  def logInfo(message: String): Unit = {
    Log.i(TAG, message)
    ()
  }

  def logError(message: String): Unit = {
    Log.e(TAG, message)
    ()
  }


}


object AndroidOpenCV extends CanLog {

  val DefaultAndroidState = SudokuState.DefaultState.copy(cap = 8, minHits = 20, maxSolvingDuration = 5000L)

  def init(): Unit = {
    if (OpenCVLoader.initDebug()) {
      logInfo("OpenCV initialized successfully.")
    } else {
      logError("Could not initialize OpenCV properly.")
    }
  }
}


class SudokuCapturer extends Activity with CvCameraViewListener2 with CanLog {

  var cameraView: CameraBridgeViewBase = _
  var rescanButton: Button = _
  var handler: Handler = _
  var frameNr: Int = 0
  var solution: Mat = _
  var calculationInProgress = false
  var adView: AdView = _
  val defaultLibrary: DigitLibrary = Map().withDefaultValue((Double.MaxValue, None))
  val defaultHitCounts: HitCounters = Map().withDefaultValue(Map[Int, Int]().withDefaultValue(0))
  val AdsenseKey: String = "ca-app-pub-1727389366588084/4496274256"


  var currState: SudokuState = AndroidOpenCV.DefaultAndroidState

  def initAssets(): Unit = {
    TemplateLibrary.getResourceAsStream = getAssets().open
    TemplateLibrary.templateResource = "templates.csv"
  }

  /** Called when the activity is first created. */
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    initAssets()

    Log.i(TAG, "called onCreate")
    getWindow.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON)
    setContentView(R.layout.sudoku)

    cameraView = findViewById(R.id.sudoku).asInstanceOf[CameraBridgeViewBase]
    cameraView.setCvCameraViewListener(this)
    rescanButton = findViewById(R.id.button_rescan).asInstanceOf[Button]

    rescanButton.setOnClickListener(new OnClickListener {
      override def onClick(v: View): Unit = {
        //  rescanButton.setVisibility(View.GONE)
        currState = AndroidOpenCV.DefaultAndroidState
        rescanButton.setVisibility(View.GONE)
        //  adView.setVisibility(View.GONE)
        solution = null

      }
    })


    adView = new AdView(this, AdSize.BANNER, AdsenseKey)
    adView.setGravity(Gravity.CENTER_HORIZONTAL | Gravity.TOP)
    adView.setVisibility(View.VISIBLE)
    findViewById(R.id.mainLayout).asInstanceOf[FrameLayout].addView(adView)

    val r = new AdRequest()
    adView.loadAd(r)

    handler = new Handler()
    rescanButton.setVisibility(View.GONE)
    cameraView.enableView()
  }

  override def onBackPressed(): Unit = {
    super.onBackPressed()
    //logInfo("backButton pressed")
  }

  override def onStop(): Unit = {
    super.onStop()
    Process.killProcess(Process.myPid())
  }

  override def onPause(): Unit = {
    super.onPause()
    //logInfo("onPause called")
    if (cameraView != null) cameraView.disableView()
  }

  override def onResume(): Unit = {
    super.onResume()
    // logInfo("onResume called")
    AndroidOpenCV.init()
    //OpenCVLoader.initAsync(OpenCVLoader.OPENCV_VERSION_2_4_6, this, mLoaderCallback)
    ()
  }

  override def onDestroy(): Unit = {
    super.onDestroy()
    //logInfo("onDestroy called")
    if (cameraView != null) cameraView.disableView()
  }


  def detectSudoku(inputFrame: CameraBridgeViewBase.CvCameraViewFrame): SudokuResult = {
    val frame = inputFrame.rgba()
    frameNr = frameNr + 1

    val (sudokuResult, nextState) = Await.result(SCandidate(frameNr, frame, currState, SParams()).calc, Duration.Inf)
    currState = nextState
    sudokuResult
  }

  def execOnUIThread(f: => Unit): Unit = {
    handler.post(new Runnable {
      override def run(): Unit = f
    })
    ()
  }

  def onCameraViewStarted(width: Int, height: Int) {
    //logInfo(s"onCameraViewStarted (width: $width, height: $height)")
  }

  def onCameraViewStopped() {
    // logInfo("onCameraViewStopped called")
  }

  def onCameraFrame(inputFrame: CameraBridgeViewBase.CvCameraViewFrame): Mat = {
    if (solution != null) {
      solution
    } else {
      if (!calculationInProgress) {
        calculationInProgress = true
        logInfo("starting to find sudoku ...")
        val result: Mat =
          detectSudoku(inputFrame) match {
            case s: SSuccess if s.someSolution.isDefined => {
              execOnUIThread({
                rescanButton.setVisibility(View.VISIBLE)
                // adView.setVisibility(View.VISIBLE)
              })
              solution = s.someSolution.get.solutionMat
              solution
            }
            case s: SSuccess if s.someSolution.isEmpty => {
              s.inputFrame.pipeline.inverted
            }
            case e: SFailure => e.inputFrame.pipeline.blurred
          }
        calculationInProgress = false
        result
      } else {
        logInfo("calculation in progress ...")
        inputFrame.gray
      }
    }
  }

}
