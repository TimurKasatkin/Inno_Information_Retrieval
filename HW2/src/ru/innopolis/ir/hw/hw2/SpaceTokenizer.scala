package ru.innopolis.ir.hw.hw2

/**
  * @author Timur Kasatkin 
  * @date 25.08.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object SpaceTokenizer {
	/**
	  * @return pairs of format: (token, document id, token index in document (starting with 0))
	  */
	def apply(document: Document): Iterable[(String, Int, Int)] = document.text
		.split(" ")
		.zipWithIndex
		.map({ case (token, index) => (token, document.id, index) })

}
