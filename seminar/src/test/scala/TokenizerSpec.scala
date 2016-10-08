import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Timur Kasatkin 
  * @date 26.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
class TokenizerSpec extends FlatSpec with Matchers {
	"A SimpleTokenizer" should "split given string by space" in {
		val result = SimpleTokenizer("I am a string")
		result should contain only("I", "am", "a", "string")
	}

	it should "not depend on the count of spaces" in {
		val result = SimpleTokenizer("I am         a       string with many          spaces")
		result should contain only("I", "am", "a", "string", "with", "many", "spaces")
	}

	it should "return nothing on string consists of only spaces" in {
		SimpleTokenizer("      ") shouldBe empty
	}

	it should "return nothing on empty string" in {
		SimpleTokenizer("") shouldBe empty
	}
}
