package ru.innopolis.ir.hw1

/**
  * @author Timur Kasatkin 
  * @date 19.08.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
class InvertedIndex(terms: List[(String, Int)]) {
	val dictionary: Array[(String, List[Int])] = terms
		.groupBy(_._1)
		.map(tuple => tuple._1 -> tuple._2.map(_._2).sorted)
		.toArray sortBy (_._1)

	override def toString = dictionary
		.map(tuple => s"${tuple._1} -> [${tuple._2.mkString(", ")}]")
		.mkString(System.lineSeparator)
}

object InvertedIndex {
	def apply(terms: List[(String, Int)]): InvertedIndex = new InvertedIndex(terms)
}



