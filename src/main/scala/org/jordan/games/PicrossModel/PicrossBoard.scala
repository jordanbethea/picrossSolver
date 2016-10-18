package org.jordan.games

import scala.util.Random

object PicrossBoard {
  def blankBoard(width:Int = 10, height:Int = 10):PicrossBoard = {
    new PicrossBoard(width, height)
  }

  def fullRandomBoard(width:Int = 10, height:Int = 10) :PicrossBoard = {
    new PicrossBoard(width, height, BoardMode.FullRandom, None)
  }

  def testBoard(width:Int = 10, height:Int = 10) : PicrossBoard = {
    new PicrossBoard(width, height, BoardMode.Test, None)
  }

  def blankCell(x:Int, y:Int):Cell = {
    new Cell(x, y)
  }

  def randomCell(x:Int, y:Int):Cell = {
    new Cell(x, y, Random.nextBoolean)
  }
}

class PicrossBoard (width:Int = 10, height:Int = 10, mode:BoardMode.Value = BoardMode.Blank, amountCount:Option[Int] = None){

  var solved = false

  val testMode = mode match{
    case BoardMode.Test => true
    case _ => false
  }

  val creationMethod = mode match{
    case BoardMode.Blank => PicrossBoard.blankCell(_,_)
    case BoardMode.FullRandom => PicrossBoard.randomCell(_,_)
    case BoardMode.Test => PicrossBoard.blankCell(_,_)
  }

  val cellMatrix = Array.tabulate[Cell](width, height)(creationMethod)

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
      //map creates new array by applying func, gets each col count val
      rowClues(column) = calculateClueList(cellMatrix.map{_(column)})
    }
  }

  def calculateClueList(cellArray: Array[Cell]):List[Int] = {
    def clueCounter(cellList:List[Cell], currentCount:Int, prevClues:List[Int]): List[Int] = {
      if (cellList == Nil){
        if(currentCount > 0)
          currentCount :: prevClues
        else
          prevClues
      }else if(testMode match{
        case true => cellList.head.marking == Marking.Active
        case false => cellList.head.active == true
      }){
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

  /*
  Checks if board solved - if a mismatch exists, then not solved
   */
  def checkSolved:Boolean = {
    solved = !cellMatrix.exists(_.exists(_.valueMismatch))
    solved
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

  /*
  * true if marking is not same as active - used to check for game finish
  * active cells need to be marked active, inactive just need to not be marked active
  */

  def valueMismatch:Boolean = {
    active match{
      case true => marking != Marking.Active
      case false => marking == Marking.Active
    }
  }
}

  /*

   */
object Marking extends Enumeration{
  type Marking = Value
  val Unknown, Active, Inactive = Value
}

object BoardMode extends Enumeration{
  type BoardMode = Value
  val Blank, FullRandom, Count, SetLayout, Test = Value
}