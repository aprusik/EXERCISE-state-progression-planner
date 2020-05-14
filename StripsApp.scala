import StripsWorld._
import stateSearch.AStar

object StripsApp {

  def main(args: Array[String]): Unit = {
    val weight = args(0).toDouble
    val h = args(1)

    val stripsWorld: StripsWorld = STRIPSParser.parse()
    groundPreds(stripsWorld)
    groundActs(stripsWorld)

    var alg = new AStar(stripsWorld, expand, test, heuristic, weight)
    h match {
      case "h0" =>
        alg = new AStar(stripsWorld, expand, test, _ => 0, weight)
      case "h-goal-lits" =>
        alg = new AStar(stripsWorld, expand, test, heuristic, weight)
      case "h1" =>
        throw new Exception("not implemented")
      case _ =>
        alg = new AStar(stripsWorld, expand, test, heuristic, weight)
    }
    val result = alg.find()
    if (result.isDefined) {
      result.get.printHist()
    } else println("No plan found.")
//    StripsWorld.groundedActs.foreach(println(_))
//    StripsWorld.groundedPreds.foreach(println(_))
  }
}