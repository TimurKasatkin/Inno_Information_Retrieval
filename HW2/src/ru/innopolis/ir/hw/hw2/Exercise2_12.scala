package ru.innopolis.ir.hw.hw2

/**
  * @author Timur Kasatkin 
  * @date 25.08.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object Exercise2_12 {

	val documents = List(
		Document(1, "new home sales top forecasts"),
		Document(2, "home sales rise in july"),
		Document(3, "increase in home sales in july"),
		Document(4, "july new home sales rise")
	)

	def main(args: Array[String]): Unit = {
		val index = InvertedIndexWithPositions(documents.flatMap(SpaceTokenizer(_)))
		println(index)
//		testAnd(index)
		testPhrase(index)
	}

	def testAnd(index: InvertedIndexWithPositions) = {
		println(s"new AND home: ${index.and("new", "home")}")
		println(s"home AND increase: ${index.and("home", "increase")}")
		println(s"in AND new: ${index.and("in", "new")}")
	}

	def testPhrase(index: InvertedIndexWithPositions) = {
		println("Phrase queries:")
		println(s"in home: ${index.positionalAnd("in","home")}")
		println(s"in sales: ${index.positionalAnd("in", "sales")}")
		println(s"in \\2 sales: ${index.positionalAnd("in", "sales", 2)}")
		println(s"sales july: ${index.positionalAnd("sales", "july")}")
	}
}
