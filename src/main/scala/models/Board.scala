package models

import scala.util.Random

class Board private (rows: Int, columns: Int)  {
  private var filledCells: List[Cell] = Nil

  def getRows: Int = rows

  def getColumns: Int = columns

  def getFilledCells: List[Cell] = this.filledCells

  def setFilledCells(filledCells: List[Cell]): Unit = {
    filledCells.foreach { cell =>
      assert(cell.getCellCoordinate.c <= columns && cell.getCellCoordinate.r <= rows && cell.getCellCoordinate.r > 0
        && cell.getCellCoordinate.c > 0)
    }

    this.filledCells = filledCells
  }

  def getRandomEmptyCellCoordinate: Option[CellCoordinate] = {
    val emptyCells = findEmptyCellCoordinates()
    val emptyCellsSize = emptyCells.size

    if(emptyCellsSize == 0) None
    else {
      val randomInt = Random.between(0, emptyCellsSize)
      Some(emptyCells(randomInt))
    }
  }

  def printBoard(): Unit = {
    for {
      r <- 1 to rows
      c <- {
        print('\n')
        1 to columns
      }
    } print(getValueAtCellCoordinate(CellCoordinate(r, c)).getOrElse("-"))

    print('\n')
  }

  //PRIVATE METHODS
  private def findEmptyCellCoordinates(): List[CellCoordinate] =
    (for {
      r <- 1 to rows
      c <- 1 to columns if getValueAtCellCoordinate(CellCoordinate(r, c)).isEmpty
    } yield CellCoordinate(r, c)).toList

  private def getValueAtCellCoordinate(cellCoordinate: CellCoordinate): Option[Int] =
    this.filledCells.find(_.getCellCoordinate == cellCoordinate).map(_.value)
}

object Board {
  def createBoard(rows: Int, columns: Int) = new Board(rows, columns)
}
