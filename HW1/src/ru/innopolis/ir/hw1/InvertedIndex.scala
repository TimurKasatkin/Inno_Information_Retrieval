package ru.innopolis.ir.hw1

import scala.collection.immutable.SortedMap
import scala.collection.mutable.ListBuffer

/**
  * @author Timur Kasatkin 
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
class InvertedIndex(terms: List[(String, Int)]) {
	private val dictionary: Map[String, List[Int]] = SortedMap(terms
		.groupBy(_._1)
		.map(tuple => tuple._1 -> tuple._2.map(_._2).distinct.sorted)
		.toArray: _*)

	override def toString = dictionary
		.map({ case (term, postings) => s"$term -> [${postings.mkString(", ")}]" })
		.mkString(System.lineSeparator)

	def and(term1: String, term2: String): List[Int] = {
		val result: ListBuffer[Int] = ListBuffer()
		val postings1 = dictionary.getOrElse(term1, List()).iterator
		val postings2 = dictionary.getOrElse(term2, List()).iterator
		var p1 = postings1.nextOrDefault
		var p2 = postings2.nextOrDefault
		while (p1.isDefined && p2.isDefined) {
			if (p1 == p2) {
				result += p1.get
				p1 = postings1.nextOrDefault
				p2 = postings2.nextOrDefault
			} else if (p1.get < p2.get)
				p1 = postings1.nextOrDefault
			else
				p2 = postings2.nextOrDefault

		}
		result.toList
	}

	def or(term1: String, term2: String): List[Int] = {
		val result: ListBuffer[Int] = ListBuffer()
		val postings1 = dictionary.getOrElse(term1, List()).iterator
		val postings2 = dictionary.getOrElse(term2, List()).iterator
		var p1 = postings1.nextOrDefault
		var p2 = postings2.nextOrDefault
		while (p1.isDefined && p2.isDefined) {
			if (p1 == p2) {
				result += p2.get
				p1 = postings1.nextOrDefault
				p2 = postings2.nextOrDefault
			} else if (p1.get < p2.get) {
				result += p1.get
				p1 = postings1.nextOrDefault
			} else {
				result += p2.get
				p2 = postings2.nextOrDefault
			}
		}
		while (p1.isDefined) {
			result += p1.get
			p1 = postings1.nextOrDefault
		}
		while (p2.isDefined) {
			result += p2.get
			p2 = postings2.nextOrDefault
		}
		result.toList
	}

	def andNot(term1: String, term2: String): List[Int] = {
		val result: ListBuffer[Int] = ListBuffer()
		val postings1 = dictionary.getOrElse(term1, List()).iterator
		val postings2 = dictionary.getOrElse(term2, List()).iterator
		var p1 = postings1.nextOrDefault
		var p2 = postings2.nextOrDefault
		while (p1.isDefined && p2.isDefined) {
			if (p1.get < p2.get) {
				result += p1.get
				p1 = postings1.nextOrDefault
			} else if (p1 == p2) {
				p1 = postings1.nextOrDefault
				p2 = postings2.nextOrDefault
			} else p2 = postings2.nextOrDefault
		}
		while (p1.isDefined) {
			result += p1.get
			p1 = postings1.nextOrDefault
		}
		result.toList
	}

	private implicit class IteratorUtils[A](iterator: Iterator[A]) {
		def nextOrDefault: Option[A] = if (iterator.hasNext) Option(iterator.next) else Option.empty
	}

}

object InvertedIndex {
	def apply(terms: List[(String, Int)]): InvertedIndex = new InvertedIndex(terms)
}



