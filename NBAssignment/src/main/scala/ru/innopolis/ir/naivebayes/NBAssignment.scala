package ru.innopolis.ir.naivebayes

import java.io.File

import scala.io.Source

/**
  * @author Timur Kasatkin 
  * @date 05.11.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
object NBAssignment {

	def main(args: Array[String]): Unit = {
		val datasetDir = new File(getClass.getClassLoader.getResource("dataset").getPath)
		assume(datasetDir.exists & datasetDir.isDirectory)

		val spamTrainFilesMap = readFolder(new File(datasetDir, "spam-train"))
		val nonSpamTrainFilesMap = readFolder(new File(datasetDir, "nonspam-train"))

		val terms: List[(String, Int, String, String)] =
			List(spamTrainFilesMap -> "spam", nonSpamTrainFilesMap -> "nonspam")
				.flatMap { case (trainFilesMap, classLabel) => trainFilesMap
					.mapValues(SpaceTokenizer(_))
					.mapValues(tokens => tokens.foldLeft(Map.empty[String, Int].withDefaultValue(0)) {
						(countMap, word) => countMap + (word -> (countMap(word) + 1))
					})
					.flatMap({ case (fileName, tokensCountsMap) =>
						tokensCountsMap.view.map({ case (term, count) => (term, count, fileName, classLabel) })
					})
				}

		val fileNameToDocId = (spamTrainFilesMap.keys ++ nonSpamTrainFilesMap.keys).zipWithIndex.toList.toMap

		val model = MultinomialNBModel(terms.map(t => t.copy(_3 = fileNameToDocId(t._3))))

		val spamTestFilesMap = readFolder(new File(datasetDir, "spam-test"))
		val nonSpamTestFilesMap = readFolder(new File(datasetDir, "nonspam-test"))

		val accuracy = classificationAccuracy(
			trueLabels = (spamTestFilesMap ++ nonSpamTestFilesMap).values.map(SpaceTokenizer(_)).map(model.predict(_)).toList,
			predictedLabels = List.fill(spamTestFilesMap.size)("spam") ++ List.fill(nonSpamTestFilesMap.size)("nonspam")
		)


		println(s"Accuracy after training on whole train set: $accuracy")

	}

	/**
	  * @return map of format (file_name -> text)
	  */
	def readFolder(dir: File): Map[String, String] = {
		require(dir.exists && dir.isDirectory)

		dir.listFiles.map(file => {
			(file.getName, Source.fromFile(file).getLines.mkString(" "))
		}).toMap
	}
}
