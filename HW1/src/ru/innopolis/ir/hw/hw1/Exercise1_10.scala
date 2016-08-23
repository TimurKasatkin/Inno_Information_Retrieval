package ru.innopolis.ir.hw.hw1

/**
  * @author Timur Kasatkin 
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object Exercise1_10 {

	val documents = List(
		Document(1, "new home sales top forecasts"),
		Document(2, "home sales rise in july"),
		Document(3, "increase in home sales in july"),
		Document(4, "july new home sales rise")
	)

	def main(args: Array[String]): Unit = {
		val index = InvertedIndex(documents.flatMap(SpaceTokenizer(_)))
		println("Index:")
		println(index)
		println()
		println(s"forecasts OR increase: ${index.or("forecasts","increase")}")
		println(s"in OR july: ${index.or("in","july")}")
		println(s"top OR forecasts: ${index.or("top","forecasts")}")
	}
}
