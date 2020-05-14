package stateSearch

trait StateSearch[A] {
  final def find(): Option[A] = {
    val result = search()
    if (result.isDefined)
      Some(result.get.data)
    else Option.empty
  }

  protected def search(): Option[Node]

  protected abstract class Node {
    def data: A
    def depth: Int
  }
}
