package ru.innopolis.ir.hw1

import scala.collection.immutable.SortedMap

/**
  * @author Timur Kasatkin
  * @date 19.08.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
class TermDocumentIncidenceMatrix(tokens: List[(String, Int)]) {

	/**
	  * Document ids
	  */
	private val columns: Array[Int] = tokens.map(_._2).toSet.toArray

	private val matrix: Map[String, BinaryVector] = SortedMap(tokens.groupBy(_._1)
		.map({ case (term, termAndDocIdTuples) =>
			val docIds = termAndDocIdTuples.map(_._2).toSet
			(term, BinaryVector(columns.map(docId => if (docIds contains docId) '1' else '0')))
		}).toArray: _*)

	override def toString: String = {
		val maxTermLength: Int = matrix.map({ case (term, vector) => term.length }).max
		s"${" " * maxTermLength}  ${columns.mkString(" ")}${System.lineSeparator}" +
			matrix.map({ case (term, vector) =>
				s"$term${" " * (maxTermLength - term.length)}  ${vector.binaryNumString.mkString(" ")}"
			}).mkString(System.lineSeparator)
	}
}

object TermDocumentIncidenceMatrix {
	def apply(tokens: List[(String, Int)]): TermDocumentIncidenceMatrix = new TermDocumentIncidenceMatrix(tokens)
}

private class BinaryVector(binaryNumberStr: String) {
	val binaryNumString = binaryNumberStr

	def &(other: BinaryVector): BinaryVector = binary(other, {
		case (c1, c2) => and(c1, c2)
	})

	private def and(char1: Char, char2: Char): Char = if (char1 == '1' && char2 == '1') '1' else '0'

	private def binary(other: BinaryVector, operator: ((Char, Char)) => Char) =
		(this.binaryNumString.length, other.binaryNumString.length) match {
			case (sourceLen, otherLen) if sourceLen == otherLen =>
				BinaryVector((this.binaryNumString zip other.binaryNumString).map(operator))
			case (sourceLen, otherLen) if sourceLen < otherLen =>
				BinaryVector((
					("0" * (otherLen - sourceLen) + this.binaryNumString) zip other.binaryNumString
					).map(operator))
			case (sourceLen, otherLen) if sourceLen > otherLen =>
				BinaryVector((
					this.binaryNumString zip ("0" * (sourceLen - otherLen) + other.binaryNumString)
					).map(operator))
		}

	def |(other: BinaryVector): BinaryVector = binary(other, {
		case (c1, c2) => or(c1, c2)
	})

	private def or(char1: Char, char2: Char): Char = if (char1 == '0' && char2 == '0') '0' else '1'

	def not: BinaryVector = BinaryVector(binaryNumString.map(not(_)))

	private def not(char: Char): Char = if (char == '0') '1' else '0'

}

private object BinaryVector {
	def apply(binaryNumberStr: String): BinaryVector = new BinaryVector(binaryNumberStr)

	def apply(binaryNumbers: Iterable[Char]): BinaryVector = new BinaryVector(binaryNumbers.mkString)
}
