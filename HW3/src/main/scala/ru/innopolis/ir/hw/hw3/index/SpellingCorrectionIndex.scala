package ru.innopolis.ir.hw.hw3.index

/**
  * @author Timur Kasatkin 
  * @date 09.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
trait SpellingCorrectionIndex {

	protected val EDIT_DISTANCE_THRESHOLD = 2

	def corrected(q: String): List[(String, List[Int])]
}
