import models.Game.GameMove
import models.{Board, Cell, Game}

object Main extends App {
  val BASE_NUMBER = 2
  val WINNING_SCORE = 2048
  val BOARD_ROWS = 4
  val BOARD_COLUMNS = 4

  val board = Board.createBoard(BOARD_ROWS, BOARD_COLUMNS)

  val game = Game.createGame(board, BASE_NUMBER, WINNING_SCORE)

  game.printBoard()

  var isGameOver = false

  while(!isGameOver) {
    println(s"Please enter a move(0/1/2/3)")
    val moveKey = scala.io.StdIn.readInt()

    GameMove.fromKey(moveKey) match {
      case Left(th) => println(s"${th.getMessage}. Please try again.")
      case Right(gameMove) =>
        val oldFilledCells = game.getFilledCells
        game.makeMove(gameMove)

        isGameOver = game.isGameOver

        if(!isGameOver) {
          val newFilledCells = game.getFilledCells
          if(!areNewAndOldFilledCellsSame(oldFilledCells, newFilledCells)) game.addNewCellIfPossible(BASE_NUMBER)
          else println(s"Unable to move the tiles so not adding any new tile")
        }
    }
    game.printBoard()
  }

  if(game.hasPlayerWon) println(s"Congratulations!!")
  else println(s"No valid moves left. You lost!!")

  private def areNewAndOldFilledCellsSame(oldFilledCells: List[Cell], newFilledCells: List[Cell]): Boolean = {
    def checkAllElementsOfAArePresentInB(a: List[Cell], b: List[Cell]): Boolean =
      a.forall{ aElem =>
        b.exists(bElem => aElem.getCellCoordinate == bElem.getCellCoordinate && aElem.value == bElem.value)
      }

    checkAllElementsOfAArePresentInB(oldFilledCells, newFilledCells) &&
      checkAllElementsOfAArePresentInB(newFilledCells, oldFilledCells)
  }
}
