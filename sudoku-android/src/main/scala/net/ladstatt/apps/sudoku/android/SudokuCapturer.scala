package net.ladstatt.apps.sudoku.android

import _root_.android.app.Activity
import _root_.android.os.{Bundle, Handler, Process}
import _root_.android.util.Log
import _root_.android.view.View.OnClickListener
import _root_.android.view.{Gravity, View, ViewGroup}
import _root_.android.widget.{Button, FrameLayout, TextView}
import com.google.android.gms.ads.{AdRequest, AdSize, AdView, MobileAds}
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

  val defaultLibrary: DigitLibrary = Map().withDefaultValue((Double.MaxValue, None))
  val defaultHitCounts: HitCounters = Map().withDefaultValue(Map[Int, Int]().withDefaultValue(0))
  val AdUnitId: String = "ca-app-pub-1727389366588084/4496274256"
  var adView: AdView = _
  var showHint: Boolean = true
  var hint: TextView = _

  var currState: SudokuState = AndroidOpenCV.DefaultAndroidState

  def initAssets(): Unit = {
    AndroidOpenCV.init()
    TemplateLibrary.getResourceAsStream = getAssets().open
    TemplateLibrary.templateResource = "templates.csv"
  }

  def execOnUIThread(f: => Unit): Unit = {
    handler.post(new Runnable {
      override def run(): Unit = f
    })
    ()
  }

  /** Called when the activity is first created. */
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)

    initAssets()

    //getWindow.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON)

    setContentView(R.layout.sudoku_frame_layout)

    MobileAds.initialize(this, AdUnitId)



    // adView = findViewById(R.id.ad_view).asInstanceOf[AdView]

    cameraView = findViewById(R.id.sudoku).asInstanceOf[CameraBridgeViewBase]
    cameraView.setCvCameraViewListener(this)
    rescanButton = findViewById(R.id.button_rescan).asInstanceOf[Button]
    hint = findViewById(R.id.use_in_landscape).asInstanceOf[TextView]

    adView = new AdView(this)

    rescanButton.setOnClickListener(new OnClickListener {
      override def onClick(v: View): Unit = {
        //  rescanButton.setVisibility(View.GONE)
        currState = AndroidOpenCV.DefaultAndroidState
        rescanButton.setVisibility(View.GONE)
        adView.setVisibility(View.GONE)
        solution = null

      }
    })


    adView.setAdSize(AdSize.SMART_BANNER)
    adView.setAdUnitId(AdUnitId)
    adView.setVisibility(View.VISIBLE)

    import FrameLayout.LayoutParams

    val p = new LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT, Gravity.TOP)

    val adRequestBuilder = new AdRequest.Builder()
    //asdRequestBuilder.addTestDevice(AdRequest.DEVICE_ID_EMULATOR)
    // val adUnitTestId: String = "E7CB8BBA8EEEF5723B49B413253C551B"
    //adRequestBuilder.addTestDevice(adUnitTestId)
    val r = adRequestBuilder.build()
    adView.loadAd(r)

    handler = new Handler()
    rescanButton.setVisibility(View.GONE)
    cameraView.enableView()

    findViewById(R.id.mainLayout).asInstanceOf[FrameLayout].addView(adView, 2, p)

  }

  override def onBackPressed(): Unit = {
    super.onBackPressed()
  }

  override def onStop(): Unit = {
    super.onStop()
    Process.killProcess(Process.myPid())
  }

  override def onPause(): Unit = {
    super.onPause()
    if (cameraView != null) cameraView.disableView()
    if (adView != null) adView.pause()

  }

  override def onResume(): Unit = {
    super.onResume()
    AndroidOpenCV.init()
    if (adView != null) adView.resume()
    //OpenCVLoader.initAsync(OpenCVLoader.OPENCV_VERSION_2_4_6, this, mLoaderCallback)
    ()
  }

  override def onDestroy(): Unit = {
    super.onDestroy()
    if (cameraView != null) cameraView.disableView()
    if (adView != null) adView.destroy()
  }

  def onCameraViewStarted(width: Int, height: Int): Unit = {
  }

  def onCameraViewStopped() {
  }

  def onCameraFrame(inputFrame: CameraBridgeViewBase.CvCameraViewFrame): Mat = {
    if (solution != null) {
      solution
    } else {
      if (!calculationInProgress) {
        calculationInProgress = true
        execOnUIThread {
          if (showHint) {
            hint.setVisibility(View.GONE)
            adView.setVisibility(View.VISIBLE)
            adView.bringToFront()
          }
        }
        logInfo("starting to find sudoku ...")
        val result: Mat =
          detectSudoku(inputFrame) match {
            case s: SSuccess if s.someSolution.isDefined => {
              execOnUIThread({
                rescanButton.setVisibility(View.VISIBLE)
              })
              solution = s.someSolution.get.solutionMat
              solution
            }
            case s: SSuccess if s.someSolution.isEmpty => {
              s.inputFrame.pipeline.frame
            }
            case e: SFailure => e.inputFrame.pipeline.grayed
          }
        calculationInProgress = false
        result
      } else {
        logInfo("calculation in progress ...")
        inputFrame.gray
      }
    }
  }

  def detectSudoku(inputFrame: CameraBridgeViewBase.CvCameraViewFrame): SudokuResult = {
    val frame = inputFrame.rgba()
    frameNr = frameNr + 1

    val pipeline: FramePipeline = FramePipeline(frame, SParams())

    pipeline.detectRectangle match {
      case None => SFailure("No rectangle detected", SCandidate(frameNr, pipeline, SRectangle(frame, pipeline.corners, pipeline.corners), currState))
      case Some(r) =>
        val rectangle: SRectangle = SRectangle(pipeline)
        val (sudokuResult, nextState) = Await.result(SCandidate(frameNr, pipeline, rectangle, currState).calc, Duration.Inf)
        currState = nextState
        sudokuResult
    }
  }

}
