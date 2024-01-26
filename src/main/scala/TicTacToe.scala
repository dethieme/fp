import scala.io.StdIn
import scala.util.Random

@main def main(): Unit = {
  println("Herzlich Willkommen. Dein Symbol ist das X.")
  startGameLoop(3)
}

val startGameLoop = (boardWidth: Int) => {
  val initialBoard: Vector[Vector[Mark]] = Vector.fill(boardWidth, boardWidth)(EMPTY)
  drawBoardToConsole(initialBoard)

  lazy val doNextMove: (Vector[Vector[Mark]], Boolean) => Unit
              = (board: Vector[Vector[Mark]], isHumanTurn: Boolean) => {
    if (isGameFinished(board)) {
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
  println("\n   " + (1 to board.length).mkString("  "))

  board.zipWithIndex.foreach { (row: Vector[Mark], rowIndex: Int) =>
    print(s"${rowIndex + 1}  ")
    row.foreach {
      case EMPTY => print("-  ")
      case     O => print("O  ")
      case     X => print("X  ")
    }
    println()
  }

  println()
}

val isGameFinished = (board: Vector[Vector[Mark]]) => {
  (evaluateGameState(board) != EMPTY) || !board.flatten.contains(EMPTY)
}

val makeHumanMove = (board: Vector[Vector[Mark]]) => {
  lazy val getConsoleInput: () => (Int, Int) = () => {
    println("Du bist dran!")

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
    val row = random.nextInt(board.length)
    val column = random.nextInt(board.length)

    if (isValidMove(board, row, column)) {
      println("Der Computer hat gespielt.")
      (row, column)
    } else {
      generateRandomMove()
    }
  }

  val (row, column) = generateRandomMove()
  updateBoard(board, row, column, O)
}

val updateBoard = (board: Vector[Vector[Mark]], row: Int, column: Int, mark: Mark) => {
  if (isValidMove(null, row, column)) {
    board.updated(row, board(row).updated(column, mark))
  } else {
    board
  }
}

val isValidMove = (board: Vector[Vector[Mark]], row: Int, column: Int) => {
  row >= 0 && row < board.length && column >= 0 && column < board.length && board(row)(column) == EMPTY
}

val printGameResult = (board: Vector[Vector[Mark]]) => {
  println("Spielende!")

  evaluateGameState(board) match {
    case EMPTY => println("\nUnentschieden")
    case X => println("\nDu gewinnst!")
    case _ => println("\nComputer gewinnt")
  }

  drawBoardToConsole(board)
}

val evaluateGameState = (board: Vector[Vector[Mark]]) => {
  val determineWinner = (lineSum: Int) => {
    if (lineSum == -board.length) O
    else if (lineSum == board.length) X
    else EMPTY
  }

  val rowResults = board.map(row => determineWinner(row.map(_.value).sum))
  val colResults = board.transpose.map(column => determineWinner(column.map(_.value).sum))
  val diagonalResults = Seq(
    determineWinner(board.indices.map(i => board(i)(i).value).sum),
    determineWinner(board.indices.map(i => board(i)(board.length - 1 - i).value).sum)
  )

  // Concatenate all results and find first !EMPTY result
  (rowResults ++ colResults ++ diagonalResults).find(_ != EMPTY).getOrElse(EMPTY)
}
