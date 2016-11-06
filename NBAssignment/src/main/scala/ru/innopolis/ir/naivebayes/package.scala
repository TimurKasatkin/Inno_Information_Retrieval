package ru.innopolis.ir

/**
  * @author Timur Kasatkin 
  * @date 05.11.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
package object naivebayes {

	def classificationAccuracy(trueLabels: List[String], predictedLabels: List[String]) = {
		require(trueLabels.size == predictedLabels.size)
		val sameCount = trueLabels zip predictedLabels count {
			case (trueLabel, predLabel) => trueLabel == predLabel
		}
		sameCount.toDouble / trueLabels.size
	}

}
