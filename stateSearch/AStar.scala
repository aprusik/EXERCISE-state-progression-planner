package stateSearch

class AStar[A](start: A, expand: A => List[A], test: A => Boolean,
               heuristic: A => Double, w: Double)
  extends UniformCost[A](start, expand, test) {

  final override def newNode(node: UCNode, child: A): UCNode =
    UCNode(child, node.depth + 1, node.g+1, w * heuristic(node.data))
}
