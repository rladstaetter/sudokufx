package net.ladstatt.core

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Files, Path}

import org.apache.commons.io.IOUtils

import scala.io.Codec


trait ClasspathAddress extends CanLog {
  /**
   * for example, /nxs/ml/configuration.xml
   *
   * @return
   */
  def value: String

  /**
   * Loads given resource as String
   *
   * @param clazz class whose classloader is used to load the resource
   * @return
   */
  def loadAsString(clazz: Class[_], charSet: Charset = Codec.UTF8.charSet): String = {
    Option(clazz.getResourceAsStream(value)) match {
      case None =>
        throw new RuntimeException(s"Could not find resource '$value'")
      case Some(value1) => IOUtils.toString(value1, charSet)
    }
  }

  /**
   * Returns true if resource contains non ascii chars.
   *
   * @return
   */
  def containsNonAsciiChars(): Boolean = {
    IOUtils.toByteArray(getClass.getResourceAsStream(value)).exists(_ < 0)
  }

  /**
   * returns a inputstream handle to this classpath address
   *
   * @param clazz class whose classloader is used to load the resource
   * @return
   */
  def inputStream(clazz: Class[_]): InputStream = {
    val r = clazz.getResourceAsStream(value)
    require(r != null, s"Classpath address $value is pointing to a non existent resource.")
    r
  }

  /**
   * Read resource as input stream, give stream to provided function, close stream afterwards, return constructed A
   *
   * @param clazz classloader to use
   * @param f     constructor function
   * @tparam A type to return
   * @return
   */
  def applyF[A](clazz: Class[_])(f: InputStream => A): A = timeR({
    val is = inputStream(clazz)
    try {
      f(is)
    } finally {
      is.close()
    }
  }, s"constructed $value from classpath")

  def existsUsingClassLoader(clazz: Class[_]): Boolean = {
    Option(clazz.getResourceAsStream(value)).isDefined
  }

  /**
   * Provides given classpath resource as byte array. Useful for binary data
   *
   * @param clazz class loader
   * @return
   */
  def asByteArray(clazz: Class[_]): Array[Byte] = {
    if (existsUsingClassLoader(clazz)) {
      IOUtils.toByteArray(clazz.getResourceAsStream(value))
    } else {
      // logError("Could not load " + value)
      logError(s"${value} does not exist.")
      throw new RuntimeException("Could not load " + value)
    }
  }

  /**
   * Writes classpath resource to a file
   *
   * @param clazz  classloader to use
   * @param target target file
   * @param force  if set to true, given target will be overwritten (force overwrite)
   * @return
   */
  def toPath(clazz: Class[_]
             , target: Path
             , force: Boolean = true): Path = {
    if (force) {
      FileIO.toPath(target, ByteBuffer.wrap(IOUtils.toByteArray(clazz.getResourceAsStream(value))))
    } else {
      if (Files.exists(target)) {
        logTrace(s"Using existing ${target.toAbsolutePath} ...")
      } else {
        FileIO.toPath(target, ByteBuffer.wrap(IOUtils.toByteArray(clazz.getResourceAsStream(value))))
      }
    }
    target
  }

  /**
   * returns file name of this classpath address
   */
  def fileName: String = value.substring(value.lastIndexOf("/") + 1, value.length)
}
