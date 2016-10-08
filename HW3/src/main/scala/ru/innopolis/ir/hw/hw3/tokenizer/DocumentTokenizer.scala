package ru.innopolis.ir.hw.hw3.tokenizer

import chalk.text.tokenize.Tokenizer
import ru.innopolis.ir.hw.hw3.document.Document

/**
  * @author Timur Kasatkin 
  * @date 08.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object DocumentTokenizer {

	def apply(tokenizer: Tokenizer)(document: Document): Iterable[(String, Int)] = tokenizer(document.text).map((_, document.id))
}
