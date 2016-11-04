package ru.innopolis.ir.midterm.compression

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{breakable,break}

/**
  * @author Timur Kasatkin 
  * @date 09.10.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object VBEncoder {

	def main(args: Array[String]): Unit = {

		val tests = List((5, "10000101"), (824, "0000011010111000"), (214577, "000011010000110010110001"))

		for ((i, res) <- tests)
			encode(i).ensuring(_.mkString == res, s"for $i we have: ${encode(i)}")

		decode(tests.flatMap({ case (i, _) => encode(i) })).ensuring(_ == tests.map(_._1))

	}

	def encode(num: Int) = {
		var n = num
		val bytes = ListBuffer[Int]()
		breakable {
			while (true) {
				bytes.prepend(n % 128)
				if (n < 128) break
				n = n / 128
			}
		}
		bytes.update(bytes.size - 1, bytes.last + 128)
		bytes.view.map(_.toBinaryString).map(b => "0" * (8 - b.length) + b).toList
	}

	def decode(byteStream: Iterable[String]) = {
		var n = 0
		val res = ListBuffer[Int]()
		for (byte <- byteStream) {
			val num = Integer.parseInt(byte, 2)
			if (num < 128)
				n = 128 * n + num
			else {
				res += 128 * n + (num - 128)
				n = 0
			}
		}
		res.toList
	}


}
