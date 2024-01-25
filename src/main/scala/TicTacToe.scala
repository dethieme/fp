import java.util.{InputMismatchException, Random}
import scala.annotation.tailrec
import scala.io.StdIn

final val BOARD_WIDTH = 3

def startGameLoop(): Unit = {
  println("Herzlich Willkommen. Dein Symbol ist das X.")

  val initialBoard: Array[Array[Mark]] = Array.fill(BOARD_WIDTH, BOARD_WIDTH)(EMPTY)
  drawBoardToConsole(initialBoard)

  @tailrec
  def doNextMove(board: Array[Array[Mark]], isHumanTurn: Boolean): Unit = {
    if (isGameFinished(board)) {
      println("Spielende!")
      printGameResult(board)
    } else {
      val updatedBoard: Array[Array[Mark]] =
        if (isHumanTurn) {
          makeHumanMove(board)
        } else {
          makeComputerMove(board)
        }

      drawBoardToConsole(updatedBoard)
      doNextMove(updatedBoard, !isHumanTurn)
    }
  }

  doNextMove(initialBoard, isHumanTurn = true)
}

def drawBoardToConsole(board: Array[Array[Mark]]): Unit = {
  println("\n   1  2  3")

  board.zipWithIndex.foreach { case (row: Array[Mark], rowIndex: Int) =>
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

def isGameFinished(board: Array[Array[Mark]]): Boolean = {
  (evaluateGameState(board) != EMPTY) || isBoardFull(board)
}

def isBoardFull(board: Array[Array[Mark]]): Boolean = {
  !board.flatten.contains(EMPTY)
}

def makeHumanMove(board: Array[Array[Mark]]): Array[Array[Mark]] = {
  println("Du bist dran!")

  def getConsoleInput: (Int, Int) = {
    try {
      println("Gib erst die Zeile, dann die Spalte ein:")
      val row = StdIn.readInt() - 1
      val column = StdIn.readInt() - 1

      if (isValidMove(board, row, column)) {
        (row, column)
      } else {
        println("Ungültiger Zug, versuche es erneut.")
        getConsoleInput
      }
    } catch {
      case _: InputMismatchException =>
        println("Eingabefehler, versuche es erneut.")
        getConsoleInput
    }
  }

  val (row, column) = getConsoleInput
  updateBoard(board, row, column, X)
}

def makeComputerMove(board: Array[Array[Mark]]): Array[Array[Mark]] = {
  @tailrec
  def generateRandomMove: (Int, Int) = {
    val random = new Random()
    val row = random.nextInt(BOARD_WIDTH)
    val column = random.nextInt(BOARD_WIDTH)

    if (isValidMove(board, row, column)) {
      (row, column)
    } else {
      generateRandomMove
    }
  }

  val (row, column) = generateRandomMove
  println("Der Computer hat gespielt.")

  updateBoard(board, row, column, O)
}

def updateBoard(board: Array[Array[Mark]], row: Int, column: Int, mark: Mark): Array[Array[Mark]] = {
  if (isValidMove(board, row, column)) {
    board.updated(row, board(row).updated(column, mark))
  } else {
    println("Ungültiger Zug.")
    board
  }
}

def isValidMove(board: Array[Array[Mark]], row: Int, column: Int): Boolean = {
  row >= 0 && row < BOARD_WIDTH && column >= 0 && column < BOARD_WIDTH && board(row)(column) == EMPTY
}

def printGameResult(board: Array[Array[Mark]]): Unit = {
  evaluateGameState(board) match {
    case EMPTY => println("\nUnentschieden")
    case X => println("\nDu gewinnst!")
    case _ => println("\nComputer gewinnt")
  }

  drawBoardToConsole(board)
}

 def evaluateGameState(board: Array[Array[Mark]]): Mark = {
  def determineWinner(lineSum: Int): Mark = {
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
