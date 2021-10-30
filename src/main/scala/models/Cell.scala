package models

class Cell private(cellCoordinate: CellCoordinate, var value: Int) {
  def getCellCoordinate: CellCoordinate = cellCoordinate
}

object Cell {
  def createCell(cellCoordinate: CellCoordinate, value: Int) = new Cell(cellCoordinate, value)
}