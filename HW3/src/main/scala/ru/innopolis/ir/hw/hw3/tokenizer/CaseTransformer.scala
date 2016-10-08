package ru.innopolis.ir.hw.hw3.tokenizer

import chalk.text.analyze.CaseFolder
import chalk.text.transform.Transformer

/**
  * @author Timur Kasatkin 
  * @date 08.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object CaseTransformer extends Transformer {
	override def apply(terms: Iterable[String]): Iterable[String] = terms.map(CaseFolder)
}
