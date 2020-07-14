package net.ladstatt.sudoku

import java.nio.file.Paths

object Sudokus {


  private val pathPrefix = "src/test/resources/net/ladstatt/sudoku/testdata/"

  val frame1 = Paths.get(pathPrefix + "frame1.png")
  val frame2 = Paths.get(pathPrefix + "frame2.png")

  /** the real deal. Test sudoku looks like this. Ideally, the algorithm should detect exactly those numbers */
  val s1ReadyToSolve: SudokuHistory =
    SudokuHistory(
      """|608001020
         |009302580
         |000890300
         |000200090
         |300080007
         |040006000
         |003025000
         |094103200
         |070600903""".stripMargin, Sudoku.minNrOfValueHits)

  lazy val sudoku1ReadyToSolve =
    SudokuEnvironment("sudoku1-readytosolve"
      , 1
      , frame1
      , Seq(862f, 283f
        , 1240f, 339f
        , 1172f, 711f
        , 804f, 640f)
      , s1ReadyToSolve.assumeReadyToSolve
      , Parameters.defaultDigitLibrary)

  val sudokuSolved: SudokuHistory =
    SudokuHistory(
      """|638451729
         |719362584
         |452897316
         |187234695
         |326589147
         |945716832
         |863925471
         |594173268
         |271648953""".stripMargin, Sudoku.minNrOfValueHits
    )

  /* sudoku with 100% hit rate */
  val sudoku1Empty =
    SudokuEnvironment("sudoku1"
      , 1
      , frame1
      , Seq(862f, 283f
        , 1240f, 339f
        , 1172f, 711f
        , 804f, 640f)
      , s1ReadyToSolve
      , Parameters.defaultDigitLibrary)

  /** what the image processing thinks it sees for frame1 */
  /*
  lazy val sudoku1CurrentBestConfiguration =
    SudokuHistory(
      """|608000020
         |009362580
         |000890300
         |000200090
         |000080000
         |040006000
         |003025000
         |094203200
         |070600900""".stripMargin)
  */
  /*
    lazy val sudoku2CurrentBestConfiguration: SudokuHistory = SudokuHistory(
      """|608001020
         |009302580
         |000890300
         |000200090
         |300080007
         |040006000
         |003025000
         |094103200
         |070600903""".stripMargin)
    */
  /** what the image processing thinks it sees for frame2 */
  lazy val sudoku2CurrentBest =
    SudokuEnvironment("sudoku2"
      , 2
      , frame2
      , corners = Seq(
        333.0f, 192.0f
        , 719.0f, 170.0f
        , 738.0f, 571.0f
        , 349.0f, 588.0f
      ), history = SudokuHistory()
      , Parameters.defaultDigitLibrary)

  /** a map with paths to original images and coordinates of sudoku area to detect */
  lazy val validSudokus: Seq[SudokuEnvironment] = Seq(sudoku1Empty, sudoku2CurrentBest)

}
