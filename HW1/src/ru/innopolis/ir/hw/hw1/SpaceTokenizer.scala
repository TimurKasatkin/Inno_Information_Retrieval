package ru.innopolis.ir.hw.hw1

/**
  * @author Timur Kasatkin 
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object SpaceTokenizer {
	def apply(document: Document): List[(String, Int)] = document.text.split(' ').map(token => token -> document.id).toList
}
