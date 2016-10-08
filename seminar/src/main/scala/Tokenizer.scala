/**
  * @author Timur Kasatkin 
  * @date 26.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
trait Tokenizer {
	def apply(str: String): List[String]
}

object SimpleTokenizer extends Tokenizer {
	override def apply(str: String): List[String] = str.split("\\s+").filter(_.nonEmpty).toList
}
