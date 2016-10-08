package ru.innopolis.ir.hw.hw3.index

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * @param keyToValTuples assumed to be sorted on keys
  */
/*private[index]*/ class TermsBinaryTree[A](keyToValTuples: List[(String, A)]) {

	private val root: Node = buildNode(keyToValTuples, ("a", "z"), 0)

	def startWith(prefix: String): Iterable[A] = {

		null
	}

	override def toString: String = root.toString

	private def traverse(from: Node): Iterable[(String,A)] = {
		val stack = mutable.Stack[Node]()
		var node = from
		val result = ListBuffer.empty[(String,A)]
		while (stack.nonEmpty || node != null) {
			if (node != null) {
				stack.push(node)
				node match {
					case _: LeafNode =>
						node = null
					case internal: InternalNode =>
						node = internal.left
				}
			} else {
				node = stack.pop()
				node match {
					case leaf: LeafNode =>
						result += Tuple2(leaf.key,leaf.value)
					case internal: InternalNode =>
						node = internal.right
				}
			}
		}
		result
	}

	private def buildNode(keyToValTuples: List[(String, A)], parentRange: (String, String), level: Int): Node = {
		keyToValTuples.length match {
			case 1 => LeafNode(keyToValTuples.head._1, keyToValTuples.head._2, level)
			case _ =>
				var (leftSplit, rightSplit) = keyToValTuples.splitAt(keyToValTuples.length / 2)

				while (leftSplit.length > 1 && rightSplit.length > 1 &&
					(leftSplit.last._1.view zip rightSplit.head._1).takeWhile({ case (s1, s2) => s1 == s2 }).length >= (level + 1)) {
					leftSplit = leftSplit :+ rightSplit.head
					rightSplit = rightSplit.drop(1)
				}
				val leftEnd = leftSplit.last._1.take(level + 1)
				val leftRange = (parentRange._1, leftEnd)
				val rightRange = (
					leftEnd.updated(leftEnd.length - 1, (leftEnd.last + 1).toChar).toString,
					//rightSplit.head._1.take(level + 1),
					parentRange._2)
				InternalNode(
					buildNode(leftSplit, leftRange, level + 1), leftRange,
					buildNode(rightSplit, rightRange, level + 1), rightRange,
					level
				)
		}
	}

	trait Node {
		val level: Int
	}

	case class InternalNode(left: Node, leftRange: (String, String), right: Node, rightRange: (String, String), level: Int) extends Node {

		def nextNode(str: String): Node = if (leftRange._1 <= str && str <= leftRange._2) left else right

		override def toString: String = {
			val tab = "    |" * (level + 1)
			s"""Internal:
			    /$tab${leftRange._1} - ${leftRange._2}
			    /$tab    $left
			    /$tab${rightRange._1} - ${rightRange._2}
			    /$tab    $right""".stripMargin('/')
		}
	}

	case class LeafNode(key: String, value: A, level: Int) extends Node {
		override def toString: String = s"Leaf: [$key -> $value]"
	}

}

private[index] object TermsBinaryTree {
	def apply[A](keyToValTuples: List[(String, A)]): TermsBinaryTree[A] = new TermsBinaryTree(keyToValTuples)
}