package ru.innopolis.ir.hw.hw3.document

import java.io.File

import scala.io.Source.fromFile

/**
  * @author Timur Kasatkin 
  * @date 07.09.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object TxtDocumentsParser {
	def apply(filesDir: String): Iterable[Document] = {
		val d = new File(filesDir)
		if (d.exists && d.isDirectory)
			(d.listFiles() zip Stream.from(0))
				.map { case (file, id) =>
					Document(
						id,
						file.getName.stripSuffix(".txt"),
						fromFile(file, "UTF-8").getLines.mkString)
				}
		else
			Iterable.empty
	}
}
