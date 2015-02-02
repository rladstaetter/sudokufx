package net.ladstatt.core

/**
 * Created by lad on 02.02.15.
 */
object SystemEnv {

  val isX64 = System.getProperty("os.arch").contains("64")

  val runOnMac = {
    System.getProperty("os.name").toLowerCase match {
      case "mac os x" => true
      case _ => false
    }
  }

}
