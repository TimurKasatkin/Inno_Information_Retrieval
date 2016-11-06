package ru.innopolis.ir.naivebayes

/**
  * @author Timur Kasatkin 
  * @date 05.11.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object SpaceTokenizer {

	def apply(text: String): Array[String] = text.split("\\s+").filter(_.nonEmpty)

}
