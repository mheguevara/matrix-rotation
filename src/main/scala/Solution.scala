
case class Limits(up: Int, right: Int, down: Int, left: Int) {
  def squeeze: Option[Limits] = {
    val newUp = up + 1
    val newRight = right - 1
    val newDown = down - 1
    val newLeft = left + 1
    if (newDown > newUp && newRight > newLeft) {
      Some(Limits(newUp, newRight, newDown, newLeft))
    } else {
      None
    }
  }
  def totalPoints: Int = (right - left + 1) * 2 + (down - up - 1) * 2
}

case class Position(row: Int, col: Int) {

  def moveDown: Position = copy(row = row + 1)

  def moveRight: Position = copy(col = col + 1)

  def moveUp: Position = copy(row = row - 1)

  def moveLeft: Position = copy(col = col - 1)

  def rotate(limits: Limits): Position = (row, col) match {

    case (r, limits.left) if r >= limits.up && r < limits.down => moveDown

    case (limits.down, c) if c >= limits.left && c < limits.right => moveRight

    case (r, limits.right) if r <= limits.down && r > limits.up => moveUp

    case (limits.up, c) if c <= limits.right && c > limits.left => moveLeft

  }

  def inInLimits(limits: Limits): Boolean = (row, col) match {

    case (limits.up, c) if c >= limits.left && c <= limits.right => true
    case (limits.down, c) if c >= limits.left && c <= limits.right => true
    case (r, limits.left) if r >= limits.up && r <= limits.down => true
    case (r, limits.right) if r >= limits.up && r <= limits.down => true
    case _ => false

  }

}

case class Rectangle(points: List[(Position, String)], limits: Limits) {
  def rotate: Rectangle = copy(points = points.map { case (pos, str) => pos.rotate(limits) -> str} )
}

case class Matrix(points: Map[Position, String], limits: Limits) {
  override def toString: String = {

    (0 to limits.down).foldLeft(new StringBuilder()) { (sb, rowIndex) =>

      (0 to limits.right).foldLeft(sb) { (acc, colIndex) =>

        sb.append(points(Position(rowIndex, colIndex))).append(" ")

      }.append("\n")

    } toString

  }
}

object Solution {

  private implicit val positionOrdering = new Ordering[Position] {
    override def compare(x: Position, y: Position): Int = {

      if (x.row == y.row && x.col == y.col) {
        0
      } else {

        if (x.row < y.row) {
          -1
        } else if (x.row > y.row) {
          1
        } else {
          if (x.col < y.col) {
            -1
          } else {
            1
          }
        }

      }

    }
  }

  def createMatrix(lines: List[List[String]], rows: Int, cols: Int): Matrix = {

    val points = lines.zipWithIndex.foldLeft(Map.empty[Position, String]) { case (acc, (row, rowIndex)) =>

      row.zipWithIndex.foldLeft(acc) { case (accAcc, (point, colIndex)) =>

          accAcc + (Position(rowIndex, colIndex) -> point)

      }

    }

    Matrix(points, Limits(0, cols - 1, rows - 1, 0))

  }

  def getRectangles(matrix: Matrix): List[Rectangle] = {

    def helper(limits: Limits, acc: List[Rectangle] = Nil): List[Rectangle] = {

      val points = matrix.points.filter(_._1.inInLimits(limits)).toList.sortBy(_._1)

      val newAcc = Rectangle(points, limits) :: acc

      limits.squeeze.fold(
        newAcc
      )(helper(_, newAcc))

    }

    helper(matrix.limits)

  }

  def rotateMatrix(rows: Int, cols: Int, time: Int, lines: List[List[String]]): String = {

    val matrix = createMatrix(lines, rows, cols)

    val rectangles = getRectangles(matrix)

    val rotatedRectangles = rectangles map { rectangle =>

      val optimize = time % rectangle.limits.totalPoints

      (1 to optimize).foldLeft(rectangle)((r, _) => r.rotate)

    }

    val rotatedPoints = rotatedRectangles.flatMap(_.points).toMap

    val newMatrix = matrix.copy(points = rotatedPoints)

    newMatrix.toString

  }

  def findAllLimits(rows: Int, cols: Int): List[Limits] = {

    def helper(limits: Limits, acc: List[Limits] = Nil): List[Limits] = {

      val newAcc = limits::acc

      limits.squeeze.fold(
        newAcc
      )(helper(_, newAcc))

    }

    helper(Limits(0, cols - 1, rows - 1, 0))

  }
  
  def collectColumns(currentColumn: Int, 
                     currentRow: Int, 
                     cols: Int, 
                     words: Array[String], 
                     limits: List[Limits],
                     times: Int, 
                     acc: Map[Position, String]): Map[Position, String] = {
    
    if (currentColumn == cols) {
      acc
    } else {
      
      val word = words(currentColumn)
      
      val pos = Position(currentRow, currentColumn)
      
      limits.find(pos.inInLimits).fold(
        collectColumns(currentColumn + 1, currentRow, cols, words, limits, times, acc + (pos -> word))
      ) { limit =>
        
        val optimize = times % limit.totalPoints
        
        val rotatedPos = (1 to optimize).foldLeft(pos)((p, _) => p.rotate(limit))
        
        collectColumns(currentColumn + 1, currentRow, cols, words, limits, times, acc + (rotatedPos -> word))
      
      }
    }
    
  }
  
  
  def collectPoints(currentRow: Int,
                   rows: Int, 
                   cols: Int,
                   limits: List[Limits],
                   times: Int,
                   acc: Map[Position, String] = Map.empty): Map[Position, String]  = {
    
    if (currentRow == rows) {
      acc
    } else {
      val row = readLine().split(" ")
      val newAcc = collectColumns(0, currentRow, cols, row, limits, times, acc)
      collectPoints(currentRow + 1, rows, cols, limits, times, newAcc)
    }
    
  }
  
  def rotateMatrixTwo(rows: Int, cols: Int, times: Int): String = {
    
    val initialLimits = Limits(0, cols - 1, rows - 1, 0)
    
    val allLimits = findAllLimits(rows, cols)
    
    val points = collectPoints(0, rows, cols, allLimits, times)
    
    val matrix = Matrix(points, initialLimits)
    
    matrix.toString
    
  }

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val rows::cols::times::Nil = readLine.split(" ").map(_.toInt).toList
    println(rotateMatrixTwo(rows, cols, times))
    //val lines = (1 to rows).map(_ => readLine.split(" ").toList).toList
    //println(rotateMatrix(rows, cols, times, lines))
  }
}