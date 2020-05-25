package net.ladstatt.sudoku

import java.util.UUID

import javafx.application.{Application, Platform}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.{FXCollections, ListChangeListener, ObservableList}
import javafx.concurrent.Task
import javafx.event.{Event, EventHandler}
import javafx.fxml.{FXMLLoader, JavaFXBuilderFactory}
import javafx.geometry.Orientation
import javafx.scene.control._
import javafx.stage.Stage
import javafx.util.Callback
import net.ladstatt.core.CanLog

import scala.jdk.CollectionConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object NxsJfxRuntime {
  /**
   * used for thread-synchronized access to the root stage of the JavaFX application
   */
  val rootStage = new java.util.concurrent.LinkedBlockingQueue[Stage](1)

  class Initializer extends Application {
    @throws(classOf[Exception])
    override def start(primaryStage: Stage): Unit = {
      Platform.setImplicitExit(false)
      primaryStage.centerOnScreen()
      rootStage.put(primaryStage)
    }
  }

}

trait JfxUtils extends CanLog {

  val logUi = false

  def execOnUiThread(f: => Unit): Unit = {
    if (!logUi) {
      execOnUiThreadWithoutLogging(f)
    } else {
      execOnUiThreadWithLogging(f)
    }
  }

  private def execOnUiThreadWithoutLogging(f: => Unit): Unit = {
    if (!Platform.isFxApplicationThread) {
      Platform.runLater(() => f)
    } else {
      f
    }
  }

  /* useful for debugging calls to javafx gui, logUi parameter has to be set to true */
  private def execOnUiThreadWithLogging(f: => Unit): Unit = {
    val uuid = UUID.randomUUID().toString
    logTrace("exec: " + uuid)
    if (!Platform.isFxApplicationThread) {
      logTrace("sche: " + uuid)
      Platform.runLater(new Runnable {
        override def run(): Unit = {
          logTrace("start: " + uuid)
          timeR(f, uuid)
          logTrace("  end: " + uuid)
        }
      })
    } else {
      f
    }
  }

  /** Initializes JavaFX runtime */
  def initJfx(): Unit = synchronized {
    if (!isRootStageSet) {
      new Thread {
        override def run(): Unit = {
          try {
            if (!isRootStageSet) {
              logInfo("[JavaFx Initialisation] start ")
              try {
                Application.launch(classOf[NxsJfxRuntime.Initializer])
              } catch {
                case e: Throwable =>
                  // programming through side effects: if launch fails,
                  // we have already initialized JavaFx due to other means
                  // however, we still have to put a stage object to the
                  // rootStage otherwise we'll hang if we access NxsJfxRuntime.rootStage.get
                  logError("Could not launch rootStage" + e.getMessage)
                  // put a dummy stage to root stage not to hang see
                  // https://nextsense.atlassian.net/browse/MNT-6168
                  execOnUiThread(NxsJfxRuntime.rootStage.put(new Stage()))
              }
            } else {
              logWarn("[JavaFx Initialisation] already running ... ")
            }
          }
          catch {
            case e: Throwable => logException(e)
          }
        }
      }.start()

      NxsJfxRuntime.rootStage.take()
      logInfo("[JavaFx Initialisation] completed ... ")
    } else {
      logWarn("[-> JAVA FX rootstage already initialized.")
    }
  }

  def isRootStageSet = !NxsJfxRuntime.rootStage.isEmpty

  /**
   * closes javafx root stage and by setting implicit exit to true it should end the javafx platform.
   *
   * This only works if no other javafx stage is currently open. alternatively, try to exit javafx by
   * calling [[Platform.exit()]]
   *
   */
  // ... best method name ever.
  def enableImplicitJavafxPlatformExit(): Unit = synchronized {
    execOnUiThread {
      Platform.setImplicitExit(true)
      if (isRootStageSet) {
        val rootStage = NxsJfxRuntime.rootStage.take()
        logInfo("[JavaFx STOP] closing root stage ... ")
        rootStage.close()
      } else {
        logError("[JavaFx STOP] no root stage set - program logic bogus!")
      }
    }
  }

  def mkObservableList[T](iterable: Iterable[T]): ObservableList[T] = {
    val mutableList = new java.util.ArrayList[T]
    mutableList.addAll(iterable.toSeq.asJava)
    FXCollections.observableList(mutableList)
  }

  def execOnUIThread(f: => Unit): Future[Unit] = Future {
    Platform.runLater(() => f)
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

  def mkFxmlLoader(fxmlResource: String): FXMLLoader = {
    val location = getClass.getResource(fxmlResource)
    require(location != null, s"Could not resolve $fxmlResource: Location was null.")
    val fxmlLoader = new FXMLLoader()
    fxmlLoader.setLocation(location)
    fxmlLoader.setBuilderFactory(new JavaFXBuilderFactory())
    fxmlLoader
  }

  def mk[A](fxmlLoader: FXMLLoader): A = {
    fxmlLoader.load()
  }

  def mkTask[X](callFn: => X): Task[X] = new Task[X] {
    override def call(): X = callFn
  }

  def as[A](xs: Seq[_]): Seq[A] = xs.map(_.asInstanceOf[A])

}

