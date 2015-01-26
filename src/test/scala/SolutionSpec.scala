import org.specs2.mutable.Specification

/**
 * Created by alaym on 26/01/15.
 */
class SolutionSpec extends Specification {

  "Solution#createMatrix" should {

    "create a matrix" in {

      val rows = 4
      val cols = 4
      val lines = List(
        List("a", "b", "c", "d"),
        List("e", "f", "g", "h"),
        List("i", "j", "k", "l"),
        List("m", "n", "o", "p")
      )

      val expected = Matrix(
        Map(
          Position(0, 0) -> "a",
          Position(0, 1) -> "b",
          Position(0, 2) -> "c",
          Position(0, 3) -> "d",
          Position(1, 0) -> "e",
          Position(1, 1) -> "f",
          Position(1, 2) -> "g",
          Position(1, 3) -> "h",
          Position(2, 0) -> "i",
          Position(2, 1) -> "j",
          Position(2, 2) -> "k",
          Position(2, 3) -> "l",
          Position(3, 0) -> "m",
          Position(3, 1) -> "n",
          Position(3, 2) -> "o",
          Position(3, 3) -> "p"
        ),
        Limits(0, 3, 3, 0)
      )

      Solution.createMatrix(lines, rows, cols) must beEqualTo(expected)

    }

  }

  "Solution#getRectangles" in {

    "get rectangles" in {

      val matrix = Matrix(
        Map(
          Position(0, 0) -> "a",
          Position(0, 1) -> "b",
          Position(0, 2) -> "c",
          Position(0, 3) -> "d",
          Position(1, 0) -> "e",
          Position(1, 1) -> "f",
          Position(1, 2) -> "g",
          Position(1, 3) -> "h",
          Position(2, 0) -> "i",
          Position(2, 1) -> "j",
          Position(2, 2) -> "k",
          Position(2, 3) -> "l",
          Position(3, 0) -> "m",
          Position(3, 1) -> "n",
          Position(3, 2) -> "o",
          Position(3, 3) -> "p"
        ),
        Limits(0, 3, 3, 0)
      )

      val expected = List(
        Rectangle(
          List(
            Position(1, 1) -> "f",
            Position(1, 2) -> "g",
            Position(2, 1) -> "j",
            Position(2, 2) -> "k"
          ),
          Limits(1, 2, 2, 1)
        ),
        Rectangle(
          List(
            Position(0, 0) -> "a",
            Position(0, 1) -> "b",
            Position(0, 2) -> "c",
            Position(0, 3) -> "d",
            Position(1, 0) -> "e",
            Position(1, 3) -> "h",
            Position(2, 0) -> "i",
            Position(2, 3) -> "l",
            Position(3, 0) -> "m",
            Position(3, 1) -> "n",
            Position(3, 2) -> "o",
            Position(3, 3) -> "p"
          ),
          Limits(0, 3, 3, 0)
        )
      )

      Solution.getRectangles(matrix) must beEqualTo(expected)

    }

  }

}
