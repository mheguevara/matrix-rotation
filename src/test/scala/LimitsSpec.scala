import org.specs2.mutable.Specification

/**
 * Created by alaym on 26/01/15.
 */
class LimitsSpec extends Specification {

  "Limits" should {

    "squeeze" in {

      val limits = Limits(0, 4, 4, 0)

      val expected = Some(
        Limits(1, 3, 3, 1)
      )

      limits.squeeze must beEqualTo(expected)

    }

    "not squeeze" in {

      val limits = Limits(1, 3, 3, 1)

      val expected = None

      limits.squeeze must beEqualTo(expected)

    }

  }

}
