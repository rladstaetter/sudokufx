package net.ladstatt.apps.sudoku

import javafx.beans.property.SimpleObjectProperty
import javafx.concurrent.{Service, Task, WorkerStateEvent}

import net.ladstatt.JavaFXUnitTest
import net.ladstatt.core.{CanLog, Utils}
import net.ladstatt.jfx.JfxUtils
import org.junit.Test
import org.opencv.core.Mat

/**
 * Created by lad on 21.09.14.
 */
class ServiceWorkerTest extends JavaFXUnitTest with JfxUtils with Utils with CanLog {

  val solverService = new SolverService
  val sudoku1 =
    """245981376
      |169273584
      |837564219
      |976125438
      |513498627
      |482736951
      |391657842
      |728349165
      |654812793""".stripMargin.replaceAll("\n", "")


  class SolverService extends Service[SudokuDigitSolution] {
    val sudokuAsString =
      """000020600
        |000005001
        |000006400
        |008102900
        |000000008
        |006708200
        |002609500
        |800203009
        |005010300""".stripMargin.replaceAll("\n", "")


    def sudoku: SudokuDigitSolution = sudokuAsString.toCharArray

    val unsolvedProperty = new SimpleObjectProperty[Mat](this, "frame")

    def getFrame = unsolvedProperty.get()

    def setFrame(frame: Mat) = unsolvedProperty.set(frame)

    setOnCancelled(mkEventHandler(
      (e: WorkerStateEvent) => {
        logInfo("CANCELLED")
      }
    ))


    override def createTask(): Task[SudokuDigitSolution] =
      new Task[SudokuDigitSolution] {
        override def call(): SudokuDigitSolution = {
          logInfo("entering")
          val someResult: Option[SudokuDigitSolution] = BruteForceSolver.solve(sudoku)
          if (isCancelled) {
            logInfo("i was cancelled")
            Thread.currentThread().interrupt()
            Array()
          } else {
            someResult match {
              case None => {
                logInfo("not found valid solution")
                Array()
              }
              case Some(s) => {
                logInfo("task finished successfully")
                s
              }
            }
          }
        }
      }
  }

  @Test def testSimple() = {
    solverService.start()
    for (i <- (1 to 10))
      time({
        Thread.sleep(50)
        solverService.cancel()
        logInfo("cancelled task.")
        solverService.restart()
        Thread.sleep(10)
      }, t => logInfo(s"Finished turn in $t ms."))

    Thread.sleep(1000)
  }
}

