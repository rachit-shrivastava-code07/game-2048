package models

import models.Game.GameMove

class Game private(board: Board, var currentMaxScore: Int, winningScore: Int) {
  def makeMove(move: GameMove): Unit = move match {
    case GameMove.UpGm => makeUpMove()
    case GameMove.DownGm => makeDownMove()
    case GameMove.LeftGm => makeLeftMove()
    case GameMove.RightGm => makeRightMove()
  }

  def getFilledCells: List[Cell] = board.getFilledCells

  def addNewCellIfPossible(cellValue: Int): Unit = {
    val randomEmptyCellCoordinateOpt = board.getRandomEmptyCellCoordinate
    randomEmptyCellCoordinateOpt match {
      case Some(emptyCellCoordinate) =>
        val newEmptyCell = Cell.createCell(emptyCellCoordinate, cellValue)
        this.board.setFilledCells(this.board.getFilledCells :+ newEmptyCell)

      case None => (): Unit
    }
  }

  def isGameOver: Boolean = hasPlayerWon || (!hasEmptyCell && !hasAtleast1AdjacentTileMergeable)

  def hasPlayerWon: Boolean = currentMaxScore == winningScore

  def printBoard(): Unit = board.printBoard()

  //PRIVATE METHODS
  private def hasAtleast1AdjacentTileMergeable: Boolean = false //TODO: Implement this

  private def hasEmptyCell: Boolean = board.getRandomEmptyCellCoordinate.isDefined

  private def getNewFilledValues(sortedCellValues: List[Int]): List[Int] = {
    var newFilledValues: List[Int] = Nil
    var i = 0

    while(i < sortedCellValues.size) {
      if(i+1 < sortedCellValues.size && sortedCellValues(i) == sortedCellValues(i+1)) {
        newFilledValues = newFilledValues :+ (sortedCellValues(i) * 2)
        i+=2
      } else {
        newFilledValues = newFilledValues :+ sortedCellValues(i)
        i+=1
      }
    }

    newFilledValues
  }

  private def setMaxScore(newFilledValues: List[Int]): Unit = {
    if(newFilledValues.nonEmpty) {
      val newMaxScore = newFilledValues.max
      currentMaxScore = newMaxScore
    } else (): Unit
  }

  private def moveVertical(sortingFn: Cell => Int)
                          (getRFn: Int => Int): Unit = {
    var newFilledCells: List[Cell] = Nil

    for {
      c <- 1 to board.getColumns
    } yield {
      val cellsInThisColumn = board.getFilledCells.filter(_.getCellCoordinate.c == c)
      val sortedCellValues = cellsInThisColumn.sortBy(sortingFn).map(_.value)

      val newFilledValues = getNewFilledValues(sortedCellValues)

      setMaxScore(newFilledValues)

      newFilledCells = newFilledCells ++ newFilledValues.zipWithIndex.map { case (value, index) =>
        val r = getRFn(index)
        Cell.createCell(CellCoordinate(r, c), value)
      }
    }

    this.board.setFilledCells(newFilledCells)
  }

  private def moveHorizontal(sortingFn: Cell => Int)
                            (getCFn: Int => Int): Unit = {
    var newFilledCells: List[Cell] = Nil

    for {
      r <- 1 to board.getRows
    } yield {
      val cellsInThisRow = board.getFilledCells.filter(_.getCellCoordinate.r == r)
      val sortedCellValues = cellsInThisRow.sortBy(sortingFn).map(_.value)

      val newFilledValues = getNewFilledValues(sortedCellValues)

      setMaxScore(newFilledValues)

      newFilledCells = newFilledCells ++ newFilledValues.zipWithIndex.map { case (value, index) =>
        val c = getCFn(index)
        Cell.createCell(CellCoordinate(r, c), value)
      }
    }

    this.board.setFilledCells(newFilledCells)
  }

  private def makeUpMove(): Unit = {
    val sortingFn: Cell => Int = _.getCellCoordinate.r
    val getRFn: Int => Int = _ + 1

    moveVertical(sortingFn)(getRFn)
  }

  private def makeDownMove(): Unit = {
    val sortingFn: Cell => Int = Integer.MAX_VALUE - _.getCellCoordinate.r
    val getRFn: Int => Int = board.getRows - _

    moveVertical(sortingFn)(getRFn)
  }

  private def makeLeftMove(): Unit = {
    val sortingFn: Cell => Int = _.getCellCoordinate.c
    val getCFn: Int => Int = _ + 1

    moveHorizontal(sortingFn)(getCFn)
  }

  private def makeRightMove(): Unit = {
    val sortingFn: Cell => Int = Integer.MAX_VALUE - _.getCellCoordinate.c
    val getCFn: Int => Int = board.getColumns - _

    moveHorizontal(sortingFn)(getCFn)
  }
}

object Game {
  def createGame(board: Board, baseNumber: Int, winningScore: Int): Game = {
    val game = new Game(board, baseNumber, winningScore)
    //Initialize game with 2 cells filled
    game.addNewCellIfPossible(baseNumber)
    game.addNewCellIfPossible(baseNumber)

    game
  }

  sealed trait GameMove {
    def keyboardKeyMapping: Int
  }

  object GameMove {
    case object UpGm extends GameMove {
      override def keyboardKeyMapping: Int = 2
    }

    case object RightGm extends GameMove {
      override def keyboardKeyMapping: Int = 1
    }

    case object DownGm extends GameMove {
      override def keyboardKeyMapping: Int = 3
    }

    case object LeftGm extends GameMove {
      override def keyboardKeyMapping: Int = 0
    }

    def fromKey(key: Int): Either[Throwable, GameMove] = {
      if(key == UpGm.keyboardKeyMapping) Right(UpGm)
      else if(key == DownGm.keyboardKeyMapping) Right(DownGm)
      else if(key == LeftGm.keyboardKeyMapping) Right(LeftGm)
      else if(key == RightGm.keyboardKeyMapping) Right(RightGm)
      else Left(new Exception(s"Incorrect key: $key"))
    }
  }
}