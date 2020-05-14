package stateSearch

import scala.annotation.tailrec
import scala.collection.mutable

class UniformCost[A](start: A, expand: A => List[A], test: A => Boolean)
  extends StateSearch[A] {

  private var open: mutable.PriorityQueue[UCNode] =
    mutable.PriorityQueue(UCNode(start, 0, 0))(
      Ordering.by((n: UCNode) => -(n.g + n.h)))
  private val closed: mutable.HashSet[A] = mutable.HashSet(start)

  @tailrec
  protected final override def search(): Option[UCNode] = {
    val node = pop()
    if (test(node.data)) Some(node)
    else {
      val children = expand(node.data)
      for (child <- children) {
        val newChild = newNode(node, child)
        if (!closed.contains(newChild.data)) {
          closed += newChild.data
          open += newChild
        }
      }
      if (open.isEmpty) throw new IllegalStateException("Empty open list")
      search()
    }
  }

  protected def newNode(node: UCNode, child: A): UCNode =
  UCNode(child, node.depth + 1, node.g+1)

  private def pop(): UCNode = {
    val head = open.head
    open = open.tail
    head
  }

  protected case class UCNode(data: A, depth: Int, g: Int, h: Double = 0) extends Node
}