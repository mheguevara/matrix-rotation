import org.specs2.mutable.Specification

/**
 * Created by alaym on 26/01/15.
 */
class PositionSpec extends Specification{

  "Position#rotate" should {

    "rotate, case along the top" in {

      val pos = Position(0, 1)

      val limits = Limits(0, 4, 4, 0)

      val expected = Position(0, 0)

      pos.rotate(limits) must beEqualTo(expected)

    }

    "rotate, case top left edge" in {

      val pos = Position(0, 0)

      val limits = Limits(0, 4, 4, 0)

      val expected = Position(1, 0)

      pos.rotate(limits) must beEqualTo(expected)

    }

    "rotate, case along the left" in {

      val pos = Position(1, 0)

      val limits = Limits(0, 4, 4, 0)

      val expected = Position(2, 0)

      pos.rotate(limits) must beEqualTo(expected)

    }

    "rotate, case down left edge" in {

      val pos = Position(4, 0)

      val limits = Limits(0, 4, 4, 0)

      val expected = Position(4, 1)

      pos.rotate(limits) must beEqualTo(expected)

    }

    "rotate, case along the down" in {

      val pos = Position(4, 2)

      val limits = Limits(0, 4, 4, 0)

      val expected = Position(4, 3)

      pos.rotate(limits) must beEqualTo(expected)

    }

    "rotate, case down right edge" in {

      val pos = Position(4, 4)

      val limits = Limits(0, 4, 4, 0)

      val expected = Position(3, 4)

      pos.rotate(limits) must beEqualTo(expected)

    }

    "rotate, case along the right" in {

      val pos = Position(2, 4)

      val limits = Limits(0, 4, 4, 0)

      val expected = Position(1, 4)

      pos.rotate(limits) must beEqualTo(expected)

    }

    "rotate, case top right edge" in {

      val pos = Position(0, 4)

      val limits = Limits(0, 4, 4, 0)

      val expected = Position(0, 3)

      pos.rotate(limits) must beEqualTo(expected)

    }

  }

  "Position#isInLimits" should {

    "be true" in {

      val point1 = Position(0, 0)
      val point2 = Position(0, 1)
      val point3 = Position(1, 0)
      val point4 = Position(1, 1)

      val limits = Limits(0, 1, 1, 0)

      point1.inInLimits(limits) must beEqualTo(true)
      point2.inInLimits(limits) must beEqualTo(true)
      point3.inInLimits(limits) must beEqualTo(true)
      point4.inInLimits(limits) must beEqualTo(true)

    }

    "not be true" in {

      val point1 = Position(1, 1)
      val point2 = Position(1, 2)
      val point3 = Position(2, 1)
      val point4 = Position(2, 2)

      val limits = Limits(0, 4, 4, 0)

      point1.inInLimits(limits) must beEqualTo(false)
      point2.inInLimits(limits) must beEqualTo(false)
      point3.inInLimits(limits) must beEqualTo(false)
      point4.inInLimits(limits) must beEqualTo(false)

    }

  }

}
