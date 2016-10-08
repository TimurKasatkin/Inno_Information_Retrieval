package ru.innopolis.ir.hw.hw3.index

import scalax.collection.immutable.BTreeSet

/**
  * @author Timur Kasatkin 
  * @date 08.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
class PermutermSpellingCorrectionIndex private(terms: Iterable[(String, Int)]) extends SpellingCorrectionIndex {

	private val rotationsTree = BTreeSet(
		terms.view.map(_._1).toList.distinct.flatMap(term => term.rotations("").map(_ -> term)).sortBy(_._1): _*
	)

	private val standardInvertedIndex = StandardInvertedIndex(terms)

	private val minChar = Char.MinValue.toString

	private val maxChar = Char.MaxValue.toString

	override def corrected(q: String): List[(String, List[Int])] = {
		q.rotations("")
			.map(_.dropRight(2))
			.distinct
			.flatMap(r => rotationsTree.range((r + minChar, minChar), (r + maxChar, maxChar)).map(_._2))
			.distinct
			.withFilter(q.editDist(_) <= EDIT_DISTANCE_THRESHOLD)
			.map(t => (t, standardInvertedIndex(t)))
			.toList
	}

}


object PermutermSpellingCorrectionIndex {

	def apply(terms: Iterable[(String, Int)]): PermutermSpellingCorrectionIndex = new PermutermSpellingCorrectionIndex(terms)

}








