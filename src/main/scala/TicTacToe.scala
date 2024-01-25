import scala.io.StdIn
import scala.util.Random

final val BOARD_WIDTH = 3

@main def main(): Unit = startGameLoop()

val startGameLoop = () => {
  println("Herzlich Willkommen. Dein Symbol ist das X.")

  val initialBoard: Vector[Vector[Mark]] = Vector.fill(BOARD_WIDTH, BOARD_WIDTH)(EMPTY)
  drawBoardToConsole(initialBoard)

  lazy val doNextMove: (Vector[Vector[Mark]], Boolean) => Unit
              = (board: Vector[Vector[Mark]], isHumanTurn: Boolean) => {
    if (isGameFinished(board)) {
      println("Spielende!")
      printGameResult(board)
    } else {
      val updatedBoard: Vector[Vector[Mark]] =
        if (isHumanTurn) {
          makeHumanMove(board)
        } else {
          makeComputerMove(board)
        }

      drawBoardToConsole(updatedBoard)
      doNextMove(updatedBoard, !isHumanTurn)
    }
  }

  doNextMove(initialBoard, true)
}

val drawBoardToConsole = (board: Vector[Vector[Mark]]) => {
  println("\n   1  2  3")

  board.zipWithIndex.foreach { case (row: Vector[Mark], rowIndex: Int) =>
    print(s"${rowIndex + 1}  ")
    row.foreach {
      case EMPTY => print("-  ")
      case O => print("O  ")
      case X => print("X  ")
    }
    println()
  }

  println()
}

val isGameFinished = (board: Vector[Vector[Mark]]) => {
  (evaluateGameState(board) != EMPTY) || !board.flatten.contains(EMPTY)
}

val makeHumanMove = (board: Vector[Vector[Mark]]) => {
  println("Du bist dran!")

  lazy val getConsoleInput: () => (Int, Int) = () => {
    try {
      println("Gib erst die Zeile, dann die Spalte ein:")
      val row = StdIn.readInt() - 1
      val column = StdIn.readInt() - 1

      if (isValidMove(board, row, column)) {
        (row, column)
      } else {
        println("Ungültiger Zug, versuche es erneut.")
        getConsoleInput()
      }
    } catch {
      case _: NumberFormatException =>
        println("Eingabefehler, versuche es erneut.")
        getConsoleInput()
    }
  }

  val (row, column) = getConsoleInput()
  updateBoard(board, row, column, X)
}

val makeComputerMove = (board: Vector[Vector[Mark]]) => {
  lazy val generateRandomMove: () => (Int, Int) = () => {
    val random = new Random()
    val row = random.nextInt(BOARD_WIDTH)
    val column = random.nextInt(BOARD_WIDTH)

    if (isValidMove(board, row, column)) {
      (row, column)
    } else {
      generateRandomMove()
    }
  }

  val (row, column) = generateRandomMove()
  println("Der Computer hat gespielt.")

  updateBoard(board, row, column, O)
}

val updateBoard = (board: Vector[Vector[Mark]], row: Int, column: Int, mark: Mark) => {
  if (isValidMove(board, row, column)) {
    board.updated(row, board(row).updated(column, mark))
  } else {
    println("Ungültiger Zug.")
    board
  }
}

val isValidMove = (board: Vector[Vector[Mark]], row: Int, column: Int) => {
  row >= 0 && row < BOARD_WIDTH && column >= 0 && column < BOARD_WIDTH && board(row)(column) == EMPTY
}

val printGameResult = (board: Vector[Vector[Mark]]) => {
  evaluateGameState(board) match {
    case EMPTY => println("\nUnentschieden")
    case X => println("\nDu gewinnst!")
    case _ => println("\nComputer gewinnt")
  }

  drawBoardToConsole(board)
}

val evaluateGameState = (board: Vector[Vector[Mark]]) => {
  val determineWinner = (lineSum: Int) => {
    if (lineSum == -BOARD_WIDTH) O
    else if (lineSum == BOARD_WIDTH) X
    else EMPTY
  }

  val rowResults = board.map(row => determineWinner(row.map(_.value).sum))
  val colResults = board.transpose.map(column => determineWinner(column.map(_.value).sum))
  val diagonalResults = Seq(
    determineWinner(board.indices.map(i => board(i)(i).value).sum),
    determineWinner(board.indices.map(i => board(i)(BOARD_WIDTH - 1 - i).value).sum)
  )

  // Concatenate all results and find first !EMPTY result
  (rowResults ++ colResults ++ diagonalResults).find(_ != EMPTY).getOrElse(EMPTY)
}
