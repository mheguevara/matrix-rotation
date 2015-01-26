
case class Limits(up: Int, right: Int, down: Int, left: Int) {
  def squeeze: Option[Limits] = ???
}

case class Position(row: Int, col: Int) {
  def rotate(limits: Limits): Position = ???
}

case class Rectangle(points: List[(Position, String)], limits: Limits) {
  def rotate: Rectangle = ???
}

case class Matrix(points: List[(Position, String)], limits: Limits) {
  def rectangles: List[Rectangle] = ???
}

object Solution {

  implicit val positionOrdering = new Ordering[Position] {
    override def compare(x: Position, y: Position): Int = ???
  }

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
  }
}