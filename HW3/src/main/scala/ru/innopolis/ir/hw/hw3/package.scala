package ru.innopolis.ir.hw

import chalk.text.tokenize.SimpleEnglishTokenizer
import chalk.text.transform.StopWordFilter
import ru.innopolis.ir.hw.hw3.tokenizer.{CaseTransformer, WordsOnlyFilter}

/**
  * @author Timur Kasatkin 
  * @date 07.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
package object hw3 {

	val SIMPLE_WORD_TOKENIZER = SimpleEnglishTokenizer.V1() ~>
		WordsOnlyFilter ~>
		CaseTransformer

	val STOP_WORDS_TOKENIZER = SIMPLE_WORD_TOKENIZER ~> StopWordFilter("en")

	def wordFrequency(words: Iterable[String]): Map[String, Int] = words.foldLeft(Map.empty[String, Int]) {
		(count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
	}

	def estimateTime[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block // call-by-name
		val t1 = System.nanoTime()
		println(s"Elapsed time: ${t1 - t0} ns")
		result
	}


}
