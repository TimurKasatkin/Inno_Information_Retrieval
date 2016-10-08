package ru.innopolis.ir.hw.hw3.index

import scala.collection.immutable.SortedSet

/**
  * @author Timur Kasatkin 
  * @date 09.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
class BiGramSpellingCorrectionIndex private(terms: Iterable[(String, Int)]) extends SpellingCorrectionIndex {

	private val JACCARD_COEF_THRESHOLD = 0.77

	private val COMMON_BIGRAMS_THRESHOLD = 0.75

	private val bigramDictionary: Map[String, SortedSet[String]] = terms
		.toStream
		.map(_._1)
		.distinct
		.flatMap(term => term.nGrams(2).map((_, term)))
		.foldLeft(Map.empty[String, SortedSet[String]])((res, t) => res + (t._1 -> (res.getOrElse(t._1, SortedSet.empty) + t._2)))


	private val standardInvertedIndex = StandardInvertedIndex(terms)

	override def corrected(q: String): List[(String, List[Int])] = {
		val termSets = q.nGrams(2)
			.map(bigramDictionary.getOrElse(_, SortedSet.empty))
			.filter(_.nonEmpty)
			.toList
		for (termSetsCombination <- termSets.combinations((termSets.length * COMMON_BIGRAMS_THRESHOLD).ceil.toInt)) {
			for (term <- termSetsCombination.foldLeft(SortedSet.empty[String])({ case (res, set) => res | set })) {
				term
			}
		}

		null
	}

//	private def jaccardCoef(s1Bigrams: Set[String], s2: String) =


}

object BiGramSpellingCorrectionIndex {
	def apply(terms: Iterable[(String, Int)]): BiGramSpellingCorrectionIndex = new BiGramSpellingCorrectionIndex(terms)
}
