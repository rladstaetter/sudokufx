package net.ladstatt.apps.sudoku.android

import _root_.android.app.Activity
import _root_.android.os.{Bundle, Handler, Process}
import _root_.android.util.Log
import _root_.android.view.View.OnClickListener
import _root_.android.view.{Gravity, View, WindowManager}
import _root_.android.widget.{Button, FrameLayout}
import com.google.ads.{AdRequest, AdSize, AdView}
import net.ladstatt.apps.sudoku._
import org.opencv.android.CameraBridgeViewBase.CvCameraViewListener2
import org.opencv.android.{BaseLoaderCallback, CameraBridgeViewBase, LoaderCallbackInterface, OpenCVLoader}
import org.opencv.core._

import scala.concurrent.Await
import scala.concurrent.duration._

object SudokuCapturer {
  val TAG = "SudokuCapturer"
}


class SudokuCapturer extends Activity with CvCameraViewListener2 {

  import net.ladstatt.apps.sudoku.android.SudokuCapturer._

  var cameraView: CameraBridgeViewBase = _
  var rescanButton: Button = _
  var handler: Handler = _
  var frameNr: Int = 0
  var solution: Mat = _
  var calculationInProgress = false
  var adView: AdView = _
  val defaultLibrary: DigitLibrary = Map().withDefaultValue((Double.MaxValue, None))
  val defaultHitCounts: HitCounters = Map().withDefaultValue(Map[Int, Int]().withDefaultValue(0))


  var currentState: SudokuState = new SudokuState()
  var digitLibrary: DigitLibrary = defaultLibrary
  var hitCounts: HitCounters = defaultHitCounts

  def logInfo(message: String): Unit = {
    Log.i(TAG, message)
    ()
  }

  def logError(message: String): Unit = {
    Log.e(TAG, message)
    ()
  }

  val mLoaderCallback: BaseLoaderCallback = new BaseLoaderCallback(this) {
    override def onManagerConnected(status: Int) {
      status match {
        case LoaderCallbackInterface.SUCCESS => {
          Log.i(TAG, "OpenCV loaded successfully")
          cameraView.enableView()
        }
        case _ => super.onManagerConnected(status)
      }
    }
  }


  def initAssets(): Unit = {
    TemplateLoader.getResourceAsStream = getAssets().open
    TemplateLoader.templateResource = "templates.csv"
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
        digitLibrary = defaultLibrary
        hitCounts = defaultHitCounts
        currentState = new SudokuState()
        rescanButton.setVisibility(View.GONE)
        adView.setVisibility(View.GONE)
        solution = null

      }
    })

    adView = new AdView(this, AdSize.BANNER, "ca-app-pub-1727389366588084/4496274256")
    adView.setGravity(Gravity.CENTER_HORIZONTAL | Gravity.TOP)
    findViewById(R.id.mainLayout).asInstanceOf[FrameLayout].addView(adView)

    val r = new AdRequest()
    adView.loadAd(r)

    handler = new Handler()
    rescanButton.setVisibility(View.GONE)
    adView.setVisibility(View.GONE)
  }

  override def onBackPressed(): Unit = {
    super.onBackPressed()
    logInfo("backButton pressed")
  }

  override def onStop(): Unit = {
    super.onStop()
    Process.killProcess(Process.myPid())
  }

  override def onPause(): Unit = {
    super.onPause()
    logInfo("onPause called")
    if (cameraView != null) cameraView.disableView()
  }

  override def onResume(): Unit = {
    super.onResume()
    logInfo("onResume called")
    OpenCVLoader.initAsync(OpenCVLoader.OPENCV_VERSION_2_4_6, this, mLoaderCallback)
    ()
  }

  override def onDestroy(): Unit = {
    super.onDestroy()
    logInfo("onDestroy called")
    if (cameraView != null) cameraView.disableView()
  }


  def detectSudoku(inputFrame: CameraBridgeViewBase.CvCameraViewFrame): SudokuResult = {
    val input = inputFrame.rgba()
    frameNr = frameNr + 1
    val (sudokuResult, currentDigitLibrary, currentHitCounters) =
      Await.result(new SCandidate(frameNr, input).calc(currentState, digitLibrary, hitCounts, 8, 20, 5000L), Duration.Inf)
    digitLibrary = currentDigitLibrary
    hitCounts = currentHitCounters
    sudokuResult
  }

  def execOnUIThread(f: => Unit): Unit = {
    handler.post(new Runnable {
      override def run(): Unit = f
    })
    ()
  }

  def onCameraViewStarted(width: Int, height: Int) {
    logInfo("onCameraViewStarted called")
  }

  def onCameraViewStopped() {
    logInfo("onCameraViewStopped called")
  }

  def onCameraFrame(inputFrame: CameraBridgeViewBase.CvCameraViewFrame): Mat = {
    if (solution != null) {
      solution
    } else {
      if (!calculationInProgress) {
        calculationInProgress = true
        logInfo("starting to find sudoku ...")
        val result: SudokuCanvas =
          detectSudoku(inputFrame) match {
            case s: SSuccess => {
              execOnUIThread({
                rescanButton.setVisibility(View.VISIBLE)
                adView.setVisibility(View.VISIBLE)
              })
              solution = s.solutionMat
              solution
            }
            case c: SCorners => c.imageIOChain.inverted
            case e: SFailure => e.imageIoChain.blurred
          }
        calculationInProgress = false
        result
      } else {
        logInfo("calculation in progress ...")
        inputFrame.gray
      }
    }
  }

  /*
 def onCameraFrame2(inputFrame: CameraBridgeViewBase.CvCameraViewFrame): Mat = {
   if (solution != null) {
     solution
   } else {
     if (!calculationInProgress) {
       calculationInProgress = true
       Try {
         logInfo("starting to find sudoku ...")
         detectSudoku(inputFrame)
       } match {
         case Success(frameResult) => {
           if (isValidSolution(frameResult.solutionString)) {
             solution = frameResult.solution
             logInfo("found sudoku: " + frameResult.solutionString)
             //  execOnUIThread(rescanButton.setVisibility(View.VISIBLE))

           }
           execOnUIThread(updateSLabels2(history.mkValueIntermediateMatrix))
           execOnUIThread(nrDetections.setText(history.detectedNumbers.size.toString))
           // execOnUIThread(updateSLabels(frameResult.detectedCells))
           calculationInProgress = false
           frameResult.solution
         }
         case Failure(e: SudokuException) => {
           val mat = Await.result(mkMatWithCurve(inputFrame.rgba, e.curve, new Scalar(0, 255, 0), 1), Duration.Inf)
           solution = null
           calculationInProgress = false
           logInfo("couldn't find sudoku: " + e.getMessage)
           mat
         }
         case Failure(e) => {
           solution = null
           calculationInProgress = false
           //  history.reset
           //  execOnUIThread(resetSLabels())

           logInfo("sudoku error: " + e.getMessage)
           inputFrame.gray
         }
       }
     } else {
       logInfo("calculation in progress ...")
       inputFrame.gray
     }
   }
 }   */

}
