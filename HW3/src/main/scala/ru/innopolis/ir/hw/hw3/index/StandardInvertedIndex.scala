package ru.innopolis.ir.hw.hw3.index

/**
  * @author Timur Kasatkin 
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
class StandardInvertedIndex private(terms: Iterable[(String, Int)]) {

	private val dictionary: Map[String, List[Int]] = terms
		.groupBy(_._1)
		.map(tuple => tuple._1 -> tuple._2.map(_._2).toList.distinct.sorted)

	def apply(term: String): List[Int] = dictionary(term)

}

object StandardInvertedIndex {
	def apply(terms: Iterable[(String, Int)]): StandardInvertedIndex = new StandardInvertedIndex(terms)
}



