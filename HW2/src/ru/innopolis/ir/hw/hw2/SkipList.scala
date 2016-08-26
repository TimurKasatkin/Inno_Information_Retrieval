package ru.innopolis.ir.hw.hw2

import scala.collection.mutable.ListBuffer

/**
  * @author Timur Kasatkin 
  * @date 25.08.16.
  * @email aronwest001@gmail.com
  * @email t.kasatkin@innopolis.ru
  */
class SkipList[A](elements: A*) extends Iterable[SkipNode[A]] {
	val length: Int = elements.length
	private val first: Node[A] = {
		val skip = math.floor(math.sqrt(elements.length)).toInt
		var cur: Node[A] = null
		val nodes = ListBuffer.empty[Node[A]]
		for ((elem, i) <- elements.reverseIterator.zipWithIndex) {
			cur = Node(elem, cur)
			nodes.prepend(cur)
		}
		if (skip > 1)
			nodes.zipWithIndex
				.filter({ case (_, i) => i % skip == 0 && i + skip < elements.length })
				.foreach({ case (node, i) => node.skipNode = nodes(i + skip) })
		cur
	}

	override def iterator: SkipListIterator[A] = skipIterator

	def skipIterator: SkipListIterator[A] = new SkipListIterator(first)
}

object SkipList {
	def apply[A](elements: A*): SkipList[A] = new SkipList(elements: _*)
}

private case class Node[T](elem: T, next: Node[T]) {
	var skipNode: Node[T] = _

	def hasSkip = skipNode != null
}

case class SkipNode[T](elem: T, skip: Option[T]) {
	def hasSkip = skip.isDefined

}

class SkipListIterator[A](curNode: Node[A]) extends Iterator[SkipNode[A]] {

	private var current = curNode

	private var prev: Node[A] = _

	override def hasNext: Boolean = current != null

	override def next: SkipNode[A] = {
		prev = current
		current = current.next
		SkipNode(prev.elem, if (prev.hasSkip) prev.skipNode.elem.option else Option.empty)
	}

	def hasSkip: Boolean = prev != null && prev.skipNode != null

	def skip: SkipNode[A] = {
		prev = prev.skipNode
		current = prev.next
		SkipNode(prev.elem, if (prev.hasSkip) prev.skipNode.elem.option else Option.empty)
	}

}


