import org.specs2.mutable.Specification

/**
 * Created by alaym on 26/01/15.
 */
class RectangleSpec extends Specification {

  "Rectangle" should {

    "rotate" in {

      val rect = Rectangle(
        List(
          (Position(1, 1), "a"),
          (Position(1, 2), "b"),
          (Position(2, 1), "c"),
          (Position(2, 2), "d")
        ),
        Limits(1, 2, 2, 1)
      )

      val expected = Rectangle(
        List(
          (Position(2, 1), "a"),
          (Position(1, 1), "b"),
          (Position(2, 2), "c"),
          (Position(1, 2), "d")
        ),
        Limits(1, 2, 2, 1)
      )

      rect.rotate must beEqualTo(expected)

    }

  }

}
