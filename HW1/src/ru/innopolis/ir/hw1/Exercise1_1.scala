package ru.innopolis.ir.hw1

/**
  * @author Timur Kasatkin 
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object Exercise1_1 {

	val documents = List(
		Document(1, "new home sales top forecasts"),
		Document(2, "home sales rise in july"),
		Document(3, "increase in home sales in july"),
		Document(4, "july new home sales rise")
	)

	def main(args: Array[String]): Unit = println(InvertedIndex(documents.flatMap(SpaceTokenizer(_))))
}
