package net.ladstatt

import org.junit.Rule
import java.util.concurrent.CountDownLatch

import javax.swing.SwingUtilities

import javafx.application.Platform
import javafx.embed.swing.JFXPanel

import org.junit.rules.TestRule
import org.junit.runner.Description
import org.junit.runners.model.Statement
import javafx.beans.property.SimpleBooleanProperty
import scala.util.control.NonFatal


/**
 * A JUnit {@link Rule} for running tests on the JavaFX thread and performing
 * JavaFX initialisation.  To include in your test case, add the following code:
 *
 * <pre>
 * {@literal @}Rule
 * public JavaFXThreadingRule jfxRule = new JavaFXThreadingRule()
 * </pre>
 *
 * @author Andy Till, scala port by LAD
 *
 */
class JavaFXThreadingRule extends TestRule {

  /**
   * Flag for setting up the JavaFX, we only need to do this once for all tests.
   */
  val jfxIsSetupProperty = new SimpleBooleanProperty(false)

  def getJfxIsSetup = jfxIsSetupProperty.get()

  def setJfxIsSetup(state: Boolean) = jfxIsSetupProperty.set(state)

  override def apply(statement: Statement, description: Description): Statement = {
    new OnJFXThreadStatement(statement)
  }

  class OnJFXThreadStatement(val statement: Statement) extends Statement {

    var rethrownException: Throwable = null

    @Override
    def evaluate(): Unit = {

      if (!getJfxIsSetup) {
        setupJavaFX()
        setJfxIsSetup(true)
      }

      val countDownLatch = new CountDownLatch(1)

      Platform.runLater(new Runnable() {
        override def run(): Unit = {
          try {
            statement.evaluate()
          } catch {
            case NonFatal(e) => rethrownException = e
          }
          countDownLatch.countDown()
        }
      })

      countDownLatch.await()

      // if an exception was thrown by the statement during evaluation,
      // then re-throw it to fail the test
      if (rethrownException != null) {
        throw rethrownException
      }
    }

    def setupJavaFX(): Unit = {
      //val timeMillis = System.currentTimeMillis()
      val latch = new CountDownLatch(1)

      SwingUtilities.invokeLater(new Runnable() {
        override def run(): Unit = {
          // initializes JavaFX environment
          new JFXPanel()

          latch.countDown()
        }
      })

      // System.out.println("javafx initialising...")
      latch.await()
      // System.out.println("javafx is initialised in " + (System.currentTimeMillis() - timeMillis) + "ms")
    }

  }

}
/**
 * Created by lad on 21.09.14.
 */
trait JavaFXUnitTest {
  val javafxStarterRule = new JavaFXThreadingRule

  @Rule def javaFXStarter = javafxStarterRule

}