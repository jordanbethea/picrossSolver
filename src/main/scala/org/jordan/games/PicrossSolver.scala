package org.jordan.games

import com.github.dunnololda.scage.ScageLib._
import com.github.dunnololda.scage.support.{ScageColor, Vec}


object PicrossSolverApp extends ScageScreenApp ("Picross Solver", 640,480) {

  var gameBoard = new PicrossBoard
  backgroundColor = WHITE
  var bgWhiteBool = true

  val cellHeight = 30
  val cellWidth = 30

  val gridOffsetLeft = 150
  val gridOffsetBottom = 20

  val xGridSize = 10
  val yGridSize = 10

  val textHeight = cellHeight - 10
  val buttonTextHeight = 12

  val randomizeLayoutText = "Randomize"
  val clearLayoutText = "Clear"
  val testModeText = "Test Mode"

  val ButtonxOffset = 50
  val ButtonyOffset = 20

  var randomizeTextColor = WHITE
  var randomizeBGColor = BLACK
  var clearTextColor = WHITE
  var clearBGColor = BLACK
  var testTextColor = WHITE
  var testBGColor = BLACK

  var clickPrintText = ""

  var testMode:Boolean = true

  val randomizeSize:Vec = messageBounds(randomizeLayoutText, buttonTextHeight.toFloat)
  val clearSize:Vec = messageBounds(clearLayoutText, buttonTextHeight.toFloat)
  val testSize:Vec = messageBounds(testMode, buttonTextHeight.toFloat)

  val randomizeStartPos = Vec(550, 300)
  val clearStartPos = Vec(550, 250)
  val testStartPos = Vec(550, 200)

  def checkClickOnButton(coords:Vec, pos:Vec, size:Vec): Boolean = {
    val buttonwidth = (size.x / 2) + ButtonxOffset
    val buttonheight = (size.y / 2) + ButtonyOffset
    val inButton:Boolean = coords.x < pos.x + buttonwidth &&
      coords.x > pos.x - buttonwidth &&
      coords.y < pos.y + buttonheight &&
      coords.y > pos.y - buttonheight
    inButton
  }

  def clickOnRandomizeButton(coords:Vec):Boolean = {
    checkClickOnButton(coords, randomizeStartPos, randomizeSize)
  }
  def clickOnClearButton(coords:Vec):Boolean = {
    checkClickOnButton(coords, clearStartPos, clearSize)
  }
  def clickOnTestButton(coords:Vec):Boolean = {
    checkClickOnButton(coords, testStartPos, testSize)
  }

  render {
    //updateMousePos
    drawCellGrid
    drawCells
    drawClueLists
    drawButtons
    drawSolved
  }

  def drawSolved: Unit = {
    if (gameBoard.solved)
      print("SOLVED WOOHOO", Vec(250, 250), RED)
    else
      print("Not solved yet", Vec(250, 250), GRAY)
  }

  def drawButtons: Unit = {
    drawFilledRectCentered(randomizeStartPos, (randomizeSize.x / 2) + ButtonxOffset,
      (randomizeSize.y / 2) + ButtonyOffset, randomizeBGColor)
    print(randomizeLayoutText, randomizeStartPos, buttonTextHeight.toFloat, randomizeTextColor, "center")

    drawFilledRectCentered(clearStartPos, (clearSize.x / 2) + ButtonxOffset, (clearSize.y / 2) + ButtonyOffset, clearBGColor)
    print(clearLayoutText, clearStartPos, buttonTextHeight.toFloat, clearTextColor, "center")

    drawFilledRectCentered(testStartPos, (testSize.x / 2) + ButtonxOffset, (testSize.y / 2) + ButtonyOffset, testBGColor)
    print(testModeText, testStartPos, buttonTextHeight.toFloat, testTextColor, "center")
  }

  def drawClueLists: Unit ={
    gameBoard.calculateAllClueLists
    for(row <- 0 until yGridSize){
      val cluesString = gameBoard.rowClues(row).reverse.mkString(" ")

      print(cluesString, Vec(gridOffsetLeft - 50, gridOffsetBottom + (row * cellHeight) + (cellHeight/2).floor),
        textHeight.toFloat, BLACK, "center")
    }
    for(col <- 0 until yGridSize){
      val bottomUpCounts = gameBoard.colClues(col).reverse
      val iteratorList = (0 to bottomUpCounts.size).toList

        (bottomUpCounts, iteratorList).zipped.foreach{  (count, pos) =>
            print(count, Vec(gridOffsetLeft + (col * cellWidth) + (cellWidth/2).floor,
              gridOffsetBottom + (yGridSize * cellHeight) + 50 + (pos * 20)),
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
        val cellColor = if(currentCell.active) BLACK else GRAY
        drawFilledRect(new Vec(gridOffsetLeft + (currentCell.xPos * cellWidth),
                  gridOffsetBottom + (currentCell.yPos * cellHeight) + cellHeight), //rects are drawn starting at top left corner
          cellWidth, cellHeight, cellColor)
      case Marking.Inactive =>
        drawInactiveCell(currentCell)
      case _ =>
    }
  }

  def drawInactiveCell(currentCell:Cell): Unit ={
    drawLine(
      new Vec(gridOffsetLeft + (currentCell.xPos * cellWidth),
        gridOffsetBottom + (currentCell.yPos * cellHeight)),
      new Vec(gridOffsetLeft + (currentCell.xPos * cellWidth) + cellWidth,
        gridOffsetBottom + (currentCell.yPos * cellHeight) + cellHeight),
      RED
    )

    drawLine(
      new Vec(gridOffsetLeft + (currentCell.xPos * cellWidth),
        gridOffsetBottom + (currentCell.yPos * cellHeight) + cellHeight),
      new Vec(gridOffsetLeft + (currentCell.xPos * cellWidth) + cellWidth,
        gridOffsetBottom + (currentCell.yPos * cellHeight)),
      RED
    )
  }

  def drawCellGrid {
    val largerSide = scala.math.min(xGridSize, yGridSize)
    for(x <- 0 to largerSide){
      if(x <= xGridSize)
      drawLine(new Vec((x * cellWidth) + gridOffsetLeft, gridOffsetBottom),
        new Vec((x * cellWidth) + gridOffsetLeft, yGridSize * cellHeight + gridOffsetBottom), BLACK)
      if(x <= yGridSize)
      drawLine(new Vec(gridOffsetLeft, (x * cellHeight) + gridOffsetBottom),
        new Vec((xGridSize * cellHeight) + gridOffsetLeft, (x * cellHeight) + gridOffsetBottom), BLACK)
    }
  }

  leftMouse( onBtnDown = leftClickHandler)
  rightMouse( onBtnDown = rightClickHandler)

  def updateMousePos: Unit = {
    print("Mouse pos: "+ mouseCoord.toString, Vec(0,400), BLACK)

    val xcellPos = scala.math.ceil((mouseCoord.x - gridOffsetLeft) / cellWidth).toInt

    val ycell = scala.math.ceil((mouseCoord.y - gridOffsetBottom) / cellHeight).toInt

    print("Cell pos: "+xcellPos+", "+ycell, Vec(0, 350), BLACK)

    val buttonChecks1 = "Randomize: "+clickOnRandomizeButton(mouseCoord)
    val buttonChecks2 = "Clear: "+ clickOnClearButton(mouseCoord)
    val buttonChecks3 = "Test: "+clickOnTestButton(mouseCoord)
    print(buttonChecks1, Vec(250, 400), BLACK)
    print(buttonChecks2, Vec(250, 350), BLACK)
    print(buttonChecks3, Vec(250, 300), BLACK)

    print(clickPrintText, Vec(400, 200), BLACK)
  }

 def leftClickHandler(click:Vec): Unit ={
   val (xcellPos, ycell) = cellGridFromCoord(click)
   if(xcellPos != -1 && ycell != -1)
     //gameBoard.cellMatrix(xcellPos)(ycell).leftClickBehavior
     gameBoard.leftClick(xcellPos, ycell)
   else if (clickOnRandomizeButton(click))
     randomizeBoard
   else if (clickOnClearButton(click))
      clearBoard
   else if (clickOnTestButton(click))
      switchTestMode
 }

  def randomizeBoard: Unit ={
    gameBoard = PicrossBoard.fullRandomBoard(xGridSize, yGridSize)
  }

  def clearBoard = {
    gameBoard = PicrossBoard.blankBoard(xGridSize, yGridSize)
  }

  def switchTestMode = {
    gameBoard = PicrossBoard.testBoard(xGridSize, yGridSize)
  }

  def rightClickHandler(click:Vec) = {
    val (xcellPos, ycell) = cellGridFromCoord(click)
    if(xcellPos != -1 && ycell != -1)
      gameBoard.cellMatrix(xcellPos)(ycell).rightClickBehavior
  }

  // (-1, -1) if not in grid
  def cellGridFromCoord(coords:Vec):(Int, Int) = {
    var printVal:String = "left mouse click"
    val inGrid:Boolean = (coords.x > gridOffsetLeft && coords.x < (xGridSize * cellWidth) + gridOffsetLeft) &&
      (coords.y > gridOffsetBottom && coords.y < (yGridSize * cellHeight) + gridOffsetBottom)

    if(inGrid) {
      val xcellPos = scala.math.floor((coords.x - gridOffsetLeft) / cellWidth).toInt

      val ycell = scala.math.floor((coords.y - gridOffsetBottom) / cellHeight).toInt
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