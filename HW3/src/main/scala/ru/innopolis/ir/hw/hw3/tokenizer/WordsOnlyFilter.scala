package ru.innopolis.ir.hw.hw3.tokenizer

import chalk.text.transform.{TokenType, Transformer}

/**
  * @author Timur Kasatkin 
  * @date 08.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object WordsOnlyFilter extends Transformer{
	override def apply(terms: Iterable[String]): Iterable[String] = terms.filter(TokenType.Word.matches)
}
