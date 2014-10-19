package net.ladstatt.apps.sudoku

/**
 * Created by lad on 04.10.14.
 */
object Ts {

  import net.ladstatt.apps.sudoku.Templates._

  def supermax(i: Int) = if (i > 0) 255 else 0

  def main(args: Array[String]): Unit = {
    Nine.data.foreach(println)
    println(
      s"""
        |package net.ladstatt.apps.sudoku
        |object Templates {
        |
        |object   One   {val data = Array[Int](${One.data.map(supermax).mkString(",")})}
        |object   Two   {val data = Array[Int](${Two.data.map(supermax).mkString(",")})}
        |object   Three {val data = Array[Int](${Three.data.map(supermax).mkString(",")})}
        |object   Four  {val data = Array[Int](${Four.data.map(supermax).mkString(",")})}
        |object   Five  {val data = Array[Int](${Five.data.map(supermax).mkString(",")})}
        |object   Six   {val data = Array[Int](${Six.data.map(supermax).mkString(",")})}
        |object   Seven {val data = Array[Int](${Seven.data.map(supermax).mkString(",")})}
        |object   Eight {val data = Array[Int](${Eight.data.map(supermax).mkString(",")})}
        |object   Nine  {val data = Array[Int](${Nine.data.map(supermax).mkString(",")})}
        |}""".stripMargin)
  }

}
