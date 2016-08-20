package ru.innopolis.ir.hw1

/**
  * @author Timur Kasatkin 
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object Exercise1_2 {

	val documents = List(
		Document(1, "breakthrough drug for schizophrenia"),
		Document(2, "new schizophrenia drug"),
		Document(3, "new approach for treatment of schizophrenia"),
		Document(4, "new hopes for schizophrenia patients")
	)

	def main(args: Array[String]): Unit = {
		val tokens = documents.flatMap(SpaceTokenizer(_))
		println("a. Term-document incidence matrix:")
		println()
		println(TermDocumentIncidenceMatrix(tokens))
		println()
		println("b. Inverted index representation:")
		println()
		println(InvertedIndex(tokens))
	}
}
