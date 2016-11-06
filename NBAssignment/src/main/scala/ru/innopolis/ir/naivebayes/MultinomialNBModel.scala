package ru.innopolis.ir.naivebayes

import scala.collection.{Map, Set, mutable => m}
import scala.math.log

/**
  * @author Timur Kasatkin
  * @date 04.11.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
/**
  *
  * @param terms list of terms in format (term, number of times it occurred in doc, doc_id, class_label)
  * @param alpha smoothing parameter
  */
class MultinomialNBModel(terms: Iterable[(String, Int, Int, String)], alpha: Int = 1) {

	val (classProbabilities: Map[String, Double],
	conditionalProbabilities: Map[String, Map[String, Double]],
	classes: Set[String]) = {
		val vocabulary = m.HashSet.empty[String]

		val classToDocIds = m.Map.empty[String, m.HashSet[Int]].withDefault(_ => m.HashSet.empty)

		val classToTermCount = m.Map.empty[String, m.Map[String, Int]]
			.withDefault(_ => m.Map.empty.withDefaultValue(0))

		for ((term, times, docId, classLabel) <- terms) {
			vocabulary += term
			classToDocIds(classLabel) = classToDocIds(classLabel) + docId
			classToTermCount(classLabel) = classToTermCount(classLabel)
			classToTermCount(classLabel)(term) = classToTermCount(classLabel)(term) + times
		}

		val numOfDocs = classToDocIds.map(_._2.size).sum

		(classToDocIds.mapValues(_.size.toDouble / numOfDocs),
			classToTermCount.mapValues(termToCount => {
				val denominator = termToCount.values.sum + alpha * vocabulary.size
				termToCount.mapValues(termsCount => (termsCount + alpha).toDouble / denominator)
			}),
			classToDocIds.keySet)
	}

	def predict(tokens: Iterable[String]) = {
		val prediction = classes.map(classLabel => (classLabel,
			log(classProbability(classLabel)) +
				tokens.map(token => {
					val condProb = conditionalProbability(classLabel, token)
					log(if (condProb == 0) 1 else condProb)
				}).sum)
		).maxBy(_._2)._1

		prediction
	}

	private def classProbability(classLabel: String): Double = classProbabilities(classLabel)

	private def conditionalProbability(classLabel: String, term: String): Double =
		conditionalProbabilities(classLabel).getOrElse(term, 0)
}

object MultinomialNBModel {
	def apply(terms: Iterable[(String, Int, Int, String)], alpha: Int = 1): MultinomialNBModel =
		new MultinomialNBModel(terms, alpha)
}
