package org.jordan.games

class PicrossBoard (width:Int = 10, height:Int = 10){

  val cellMatrix = Array.tabulate[Cell](width, height)((x:Int, y:Int) => new Cell(x, y))

  val rowClues = Array.fill[List[Int]](height)(Nil)
  val colClues = Array.fill[List[Int]](width)(Nil)

  def activateCell(x:Int, y:Int): Unit ={
    cellMatrix(x)(y).activate
  }
  def inactivateCell(x:Int, y:Int) = {
    cellMatrix(x)(y).deactivate
  }

  def markFilled(x:Int, y:Int): Unit = { cellMatrix(x)(y).markActive }
  def markEmpty(x:Int, y:Int): Unit = {cellMatrix(x)(y).markInactive }
  def delete(x:Int, y:Int){ cellMatrix(x)(y).blank }

  def calculateAllClueLists: Unit ={
    for(row <- 0 until height){
      colClues(row) = calculateClueList(cellMatrix(row))
    }
    for(column <- 0 until width){
      rowClues(column) = calculateClueList(cellMatrix.map{_(column)}) //map creates new array by applying func, gets each col count val
    }
  }

  def calculateClueList(cellArray: Array[Cell]):List[Int] = {
    def clueCounter(cellList:List[Cell], currentCount:Int, prevClues:List[Int]): List[Int] = {
      if (cellList == Nil){
        if(currentCount > 0)
          currentCount :: prevClues
        else
          prevClues
      }else if(cellList.head.marking == Marking.Active){
        clueCounter(cellList.tail, currentCount + 1, prevClues) //current cell is active, add 1 to clue value
      }else{
        if(currentCount > 0)
          clueCounter(cellList.tail, 0, currentCount :: prevClues) //inactive cell, prev count, end clue, start over, add to list
        else
          clueCounter(cellList.tail, 0, prevClues) //inactive cell, no prev count, move on doing nothing
      }
    }
    val cellList = cellArray.toList
    return clueCounter(cellList, 0, Nil)
  }

}

case class Cell (val xPos:Int,
            val yPos:Int,
            var active:Boolean = false,
            var marking:Marking.Marking = Marking.Unknown){

  def activate = {active = true}
  def deactivate = {active = false}

  def leftClickBehavior = {
    if(marking == Marking.Unknown)
      marking = Marking.Active
    else
      marking = Marking.Unknown
  }

  def rightClickBehavior = {
    if(marking == Marking.Unknown)
      marking = Marking.Inactive
    else
      marking = Marking.Unknown
  }

  def markActive = {
    if(marking == Marking.Inactive)
      marking = Marking.Unknown
    else
      marking = Marking.Active
  }
  def markInactive = {
    if(marking == Marking.Active)
      marking = Marking.Unknown
    else
      marking = Marking.Inactive
  }
  def blank = { marking = Marking.Unknown}
}

  /*

   */
object Marking extends Enumeration{
  type Marking = Value
  val Unknown, Active, Inactive = Value
}