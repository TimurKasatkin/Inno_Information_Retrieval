package ru.innopolis.ir.hw.hw2

import java.lang.System.{lineSeparator => lineSep}

import scala.collection.immutable.SortedMap
import scala.collection.mutable.ListBuffer
import scala.math.abs
import scala.util.control.Breaks._

/**
  * @author Timur Kasatkin 
  * @date 25.08.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
/**
  * @param terms - collection of pairs (token, document_id, token index in the document (starting with 0) )
  */
class InvertedIndexWithPositions(terms: Iterable[(String, Int, Int)]) {

	val dictionary: Map[String, SkipList[(Int, List[Int])]] = SortedMap(terms
		.groupBy(_._1)
		.map({ case (term, tuples) =>
			(term, SkipList(tuples
				.map({ case (_, docId, inDocIndex) => (docId, inDocIndex) })
				.groupBy(_._1)
				.map({ case (docId, docIdToInDocIndexPairs) => (docId, docIdToInDocIndexPairs.map(_._2).toList) })
				.toSeq
				.sortBy(_._1)
				.toList: _*)
				)
		}).toSeq: _*)

	override def toString = dictionary
		.map({ case (term, postings) =>
			s"$term, ${postings.length} -> $lineSep" +
				postings.map(_.elem).map({ case (docId, indexes) => s"      $docId: <${indexes.mkString(", ")}>" }).mkString(lineSep)
		}).mkString(lineSep)


	def and(term1: String, term2: String) = {
		val result: ListBuffer[Int] = ListBuffer.empty
		val postings1 = dictionary.getOrElse(term1, SkipList()).skipIterator
		val postings2 = dictionary.getOrElse(term2, SkipList()).skipIterator
		var (p1, p2) = (postings1.nextOrDefault, postings2.nextOrDefault)
		while (p1.isDefined && p2.isDefined) {
			val (docId1, docId2) = (p1().elem._1, p2().elem._1)
			if (docId1 == docId2) {
				result += docId1
				p1 = postings1.nextOrDefault
				p2 = postings2.nextOrDefault
			} else if (docId1 < docId2) {
				if (p1().hasSkip && p1().skip()._1 <= docId2)
					while (p1().hasSkip && p1().skip()._1 <= docId2)
						p1 = postings1.skip.option
				else
					p1 = postings1.nextOrDefault
			} else {
				if (p2().hasSkip && p2().skip()._1 <= docId1)
					while (p2().hasSkip && p2().skip()._1 <= docId1)
						p2 = postings2.skip.option
				else
					p2 = postings2.nextOrDefault
			}
		}
		result.toList
	}

	/**
	  * Find places where the two terms appear within k words of (!on either side) each other
	  *
	  * @return list of tuples of format: (docId, term1 index within doc, term2 index within doc)
	  */
	def positionalAnd(term1: String, term2: String, k: Int = 1) = {
		val result = ListBuffer.empty[(Int, Int, Int)]
		val postings1 = dictionary.getOrElse(term1, SkipList()).skipIterator
		val postings2 = dictionary.getOrElse(term2, SkipList()).skipIterator
		var (p1, p2) = (postings1.nextOrDefault, postings2.nextOrDefault)
		while (p1.isDefined && p2.isDefined) {
			val (docId1, docId2) = (p1().elem._1, p2().elem._1)
			if (docId1 == docId2) {
				val l = ListBuffer.empty[Int]
				val positions2 = p2().elem._2.iterator
				var pp2 = positions2.nextOrDefault
				for (pp1 <- p1().elem._2) {
					breakable {
						while (pp2.isDefined) {
							if (abs(pp1 - pp2()) <= k)
								l += pp2()
							else if (pp2() > pp1)
								break
							pp2 = positions2.nextOrDefault
						}
					}
					while (l.nonEmpty && abs(l.head - pp1) > k)
						l.remove(0)
					for (ps <- l)
						result += Tuple3(docId1, pp1, ps)
				}
				p1 = postings1.nextOrDefault
				p2 = postings2.nextOrDefault
			} else if (docId1 < docId2) {
				if (p1().hasSkip && p1().skip()._1 <= docId2)
					while (p1().hasSkip && p1().skip()._1 <= docId2)
						p1 = postings1.skip.option
				else
					p1 = postings1.nextOrDefault
			} else {
				if (p2().hasSkip && p2().skip()._1 <= docId1)
					while (p2().hasSkip && p2().skip()._1 <= docId1)
						p2 = postings2.skip.option
				else
					p2 = postings2.nextOrDefault
			}
		}
		result.toList
	}

	private implicit class IteratorUtils[A](iterator: Iterator[A]) {
		def nextOrDefault: Option[A] = if (iterator.hasNext) iterator.next.option else Option.empty
	}

}

object InvertedIndexWithPositions {
	def apply(terms: Iterable[(String, Int, Int)]): InvertedIndexWithPositions = new InvertedIndexWithPositions(terms)
}
