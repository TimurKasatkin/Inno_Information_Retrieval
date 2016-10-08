package ru.innopolis.ir.hw.hw3

import scala.math._

/**
  * @author Timur Kasatkin 
  * @date 09.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
package object index {

	/**
	  * https://gist.github.com/tixxit/1246894/e79fa9fbeda695b9e8a6a5d858b61ec42c7a367d
	  */
	def editDistance[A](a: Iterable[A], b: Iterable[A]) = ((0 to b.size).toList /: a) ((prev, x) =>
		(prev zip prev.tail zip b).scanLeft(prev.head + 1) {
			case (h, ((d, v), y)) => min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
		}) last

	implicit class StringUtils(string: String) {
		def editDist(other: String): Int = editDistance(string, other)

		def rotations(sep: String = "$") = (0 to string.length)
			.map(i => string.drop(i) + sep + string.take(i))
			.distinct

		def nGrams(n: Int) = string.sliding(n)
	}

}
