package ru.innopolis.ir.hw

/**
  * @author Timur Kasatkin 
  * @date 26.08.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
package object hw2 {

	implicit class ObjectUtils[A](a: A) {
		def option: Option[A] = Option(a)
	}

	implicit class OptionUtils[A](option: Option[A]) {
		def apply(): A = option.get
	}
}
