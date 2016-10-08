package ru.innopolis.ir.hw.hw3

import java.lang.System.{lineSeparator => lineSep}

import ru.innopolis.ir.hw.hw3.document.TxtDocumentsParser
import ru.innopolis.ir.hw.hw3.index.{BiGramSpellingCorrectionIndex, PermutermSpellingCorrectionIndex}
import ru.innopolis.ir.hw.hw3.tokenizer.DocumentTokenizer

import scala.collection.IterableView

/**
  * @author Timur Kasatkin 
  * @date 07.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object QueryHandlingComparison {

	val NUM_OF_FREQ_TERM = 20

	def main(args: Array[String]): Unit = {
		//Tokenization
		val docs = TxtDocumentsParser(getClass.getClassLoader.getResource("articles").getPath)
		val tokens = docs.view.flatMap(DocumentTokenizer(STOP_WORDS_TOKENIZER))
		val docsMap = docs.map(d => d.id -> d).toMap
		val nMostFrequentWords = wordFrequency(tokens.map(_._1))
			.toSeq
			.sortBy(-_._2)
			.take(NUM_OF_FREQ_TERM)
			.map(_._1)
			.toSet
		val filteredTokens = tokens.filterNot({ case (token, _) => nMostFrequentWords.contains(token) }).toList

		//Indexing
		val permutermSpellingCorrectionIndex = estimateTime(PermutermSpellingCorrectionIndex(filteredTokens))
		val kGramSpellingCorrectionIndex = estimateTime(BiGramSpellingCorrectionIndex(filteredTokens))

		def printResult(spellCorrectionResult: List[(String, List[Int])]) = {
			println(spellCorrectionResult.map({ case (word, docIds) => (word, docIds.view.map(docsMap).map(_.title).sorted.force) }).mkString(lineSep))
		}

		printResult(permutermSpellingCorrectionIndex.corrected("prosttics"))

//		printResult(kGramSpellingCorrectionIndex.corrected("prosttics"))
	}
}
