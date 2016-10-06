package org.jordan.games

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scage.support.{ScageColor, Vec}


object PicrossSolverApp extends ScageScreenApp ("Picross Solver", 640,480) {

  val gameBoard = new PicrossBoard
  backgroundColor = WHITE
  var bgWhiteBool = true

  val cellHeight = 30
  val cellWidth = 30

  val offsetLeft = 150
  val offsetBottom = 20

  val xGridSize = 10
  val yGridSize = 10

  val textHeight = cellHeight - 10

  var clickPrintText = ""

  render {
//    updateMousePos
    drawCellGrid
    drawCells
    drawClueLists
  }

  def drawClueLists: Unit ={
    gameBoard.calculateAllClueLists
    for(row <- 0 until yGridSize){
      val cluesString = gameBoard.rowClues(row).mkString(" ")
/*      val cluesList = gameBoard.rowClues(row)
      val cluesString = cluesList match {
        case Nil => ""
        case x => x.toString
      } */

      print(cluesString, Vec(offsetLeft - 50, offsetBottom + (row * cellHeight) + (cellHeight/2).floor),
        textHeight.toFloat, BLACK, "center")
    }
    for(col <- 0 until yGridSize){
      val bottomUpCounts = gameBoard.colClues(col).reverse
      val iteratorList = (0 to bottomUpCounts.size).toList

        (bottomUpCounts, iteratorList).zipped.foreach{  (count, pos) =>
            print(count, Vec(offsetLeft + (col * cellWidth) + (cellWidth/2).floor,
              offsetBottom + (yGridSize * cellHeight) + 50 + (pos * 20)),
              textHeight.toFloat, BLACK, "center")
        }
    }
  }

  def drawCells: Unit ={
    gameBoard.cellMatrix.foreach(_.foreach(drawCell(_)))
  }

  def drawCell(currentCell:Cell): Unit ={
    currentCell.marking match {
      case Marking.Active =>
        drawFilledRect(new Vec(offsetLeft + (currentCell.xPos * cellWidth),
                  offsetBottom + (currentCell.yPos * cellHeight) + cellHeight), //rects are drawn starting at top left corner
          cellWidth, cellHeight, BLACK)
      case Marking.Inactive =>
        drawInactiveCell(currentCell)
      case _ =>
    }
  }

  def drawInactiveCell(currentCell:Cell): Unit ={
    drawLine(
      new Vec(offsetLeft + (currentCell.xPos * cellWidth),
        offsetBottom + (currentCell.yPos * cellHeight)),
      new Vec(offsetLeft + (currentCell.xPos * cellWidth) + cellWidth,
        offsetBottom + (currentCell.yPos * cellHeight) + cellHeight),
      RED
    )

    drawLine(
      new Vec(offsetLeft + (currentCell.xPos * cellWidth),
        offsetBottom + (currentCell.yPos * cellHeight) + cellHeight),
      new Vec(offsetLeft + (currentCell.xPos * cellWidth) + cellWidth,
        offsetBottom + (currentCell.yPos * cellHeight)),
      RED
    )
  }

  def drawCellGrid {
    val largerSide = scala.math.min(xGridSize, yGridSize)
    for(x <- 0 to largerSide){
      if(x <= xGridSize)
      drawLine(new Vec((x * cellWidth) + offsetLeft, offsetBottom),
        new Vec((x * cellWidth) + offsetLeft, yGridSize * cellHeight + offsetBottom), BLACK)
      if(x <= yGridSize)
      drawLine(new Vec(offsetLeft, (x * cellHeight) + offsetBottom),
        new Vec((xGridSize * cellHeight) + offsetLeft, (x * cellHeight) + offsetBottom), BLACK)
    }
  }

  leftMouse( onBtnDown = leftClickHandler)
  rightMouse( onBtnDown = rightClickHandler)

  def updateMousePos: Unit = {
    print("Mouse pos: "+ mouseCoord.toString, Vec(0,400), BLACK)

    val xcellPos = scala.math.ceil((mouseCoord.x - offsetLeft) / cellWidth).toInt

    val ycell = scala.math.ceil((mouseCoord.y - offsetBottom) / cellHeight).toInt

    print("Cell pos: "+xcellPos+", "+ycell, Vec(0, 350), BLACK)

    print(clickPrintText, Vec(400, 200), BLACK)
  }

 def leftClickHandler(click:Vec): Unit ={
   val (xcellPos, ycell) = cellGridFromCoord(click)
   if(xcellPos != -1 && ycell != -1)
     gameBoard.cellMatrix(xcellPos)(ycell).leftClickBehavior
 }

  def rightClickHandler(click:Vec) = {
    val (xcellPos, ycell) = cellGridFromCoord(click)
    if(xcellPos != -1 && ycell != -1)
      gameBoard.cellMatrix(xcellPos)(ycell).rightClickBehavior
  }

  // (-1, -1) if not in grid
  def cellGridFromCoord(coords:Vec):(Int, Int) = {
    var printVal:String = "left mouse click"
    val inGrid:Boolean = (coords.x > offsetLeft && coords.x < (xGridSize * cellWidth) + offsetLeft) &&
      (coords.y > offsetBottom && coords.y < (yGridSize * cellHeight) + offsetBottom)

    if(inGrid) {
      val xcellPos = scala.math.floor((coords.x - offsetLeft) / cellWidth).toInt

      val ycell = scala.math.floor((coords.y - offsetBottom) / cellHeight).toInt
      (xcellPos, ycell)
    } else{
      (-1, -1)
    }
  }

  /*
  private var ang = 0f
  private var speed = 5

  actionStaticPeriod(100) {
    ang += speed
  }

  backgroundColor = WHITE
  render {
    openglMove(windowSize/2)
    openglRotate(ang)
    print("Hello World!", Vec(-50, -5), GREEN)
  }

  leftMouse( onBtnDown = { mouseCoord =>
    speed = speed * -1
  })
  */
}