package ru.innopolis.ir.hw.hw2

import scala.util.control.Exception._

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
		testAnd(index)
		testPhraseForIndex(index)
		testPhraseFromInputForIndex(index)
	}

	def testPhraseForIndex(index: InvertedIndexWithPositions) = {
		println("Phrase queries:")
		println(s"in home: ${index.positionalAnd("in", "home")}")
		println(s"in sales: ${index.positionalAnd("in", "sales")}")
		println(s"in \\2 sales: ${index.positionalAnd("in", "sales", 2)}")
		println(s"sales july: ${index.positionalAnd("sales", "july")}")
		println()
	}

	def testPhraseFromInputForIndex(index: InvertedIndexWithPositions): Unit = {
		while (true) {
			val split: Array[String] = scala.io.StdIn.readLine("Input phrase to search and \\k or 0 to exit: ").split(" ")
			if (split(0) == "0") return
			val k = catching(classOf[NumberFormatException]).opt[Int](split.last.toInt)
			println(s"Result: ${index.positionalAnd(split(0), split(1), if (k.isDefined) k.get else 1)}")
		}
	}

	def testAnd(index: InvertedIndexWithPositions) = {
		println("Intersection queries:")
		println(s"new AND home: ${index.and("new", "home")}")
		println(s"home AND increase: ${index.and("home", "increase")}")
		println(s"in AND new: ${index.and("in", "new")}")
		println()
	}
}
