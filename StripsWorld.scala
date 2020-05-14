import STRIPSParser.{Action, Predicate}

class StripsWorld(
                   val predicates: Set[Predicate],
                   val constants: Set[String],
                   val actions: Set[Action],
                   val curState: Set[Predicate],
                   val goalState: Set[Predicate],
                   val goalNegState: Set[Predicate],
                   val history: List[Action] = List.empty
                 ) {
  StripsWorld.newStates = StripsWorld.newStates + curState

  private def isPossible(action: Action): Boolean = {
    {action.pre subsetOf curState} &&
      action.preNeg.forall(p => !curState.contains(p))
  }

  private def isFinished: Boolean = {
    {goalState subsetOf curState} &&
      goalNegState.forall(p => !curState.contains(p))
  }

  private def doAction(action: Action): StripsWorld = {
    var newState = curState
    newState = newState ++ action.add
    newState = newState -- action.del
    new StripsWorld(
      predicates,
      constants,
      actions,
      newState,
      goalState,
      goalNegState,
      history :+ action
    )
  }

  def printHist(): Unit = {
    var i: Int = 0
    history.foreach{a => println(s"$i $a"); i += 1}
    val e = StripsWorld.expanded
    val g = StripsWorld.generated
    println(s"$g nodes generated")
    println(s"$e nodes expanded")
  }
}

object StripsWorld {
  var groundedActs: Set[Action] = Set.empty
  var groundedPreds: Set[Predicate] = Set.empty
  var oldStates: Set[Set[Predicate]] = Set.empty
  var newStates: Set[Set[Predicate]] = Set.empty
  var generated: Int = 0
  var expanded: Int = 0
  def updateStates(): Unit = oldStates = newStates

  def expand(curWorld: StripsWorld): List[StripsWorld] = {
    expanded += 1
    val possibleActs = groundedActs.filter(curWorld.isPossible)
//    possibleActs.foreach(println(_))
    val possibleWorlds = possibleActs.map(curWorld.doAction).toList.filterNot(
      w => oldStates.contains(w.curState))
    generated += possibleWorlds.size
    updateStates()
    possibleWorlds
  }

  def test(world: StripsWorld): Boolean = {
    world.isFinished
  }

  def heuristic(world: StripsWorld): Double = {
    var h = 0
    world.goalState.foreach(p => if (!world.curState.contains(p)) h += 1)
    world.goalNegState.foreach(p => if (world.curState.contains(p)) h += 1)
    h
  }

  def groundPreds(world: StripsWorld): Unit = {
    val predicates = world.predicates
    val const: Array[String] = world.constants.toArray

    for (p <- predicates) {
      val size = p.variables.length
      val possible =
        ground(const, {
          for (_ <- 1 to size) yield {
            const(0)
          }
        }.toArray, size)
      possible.foreach(poss => {
        val newPred = Predicate(p.name, poss)
        groundedPreds = groundedPreds + newPred
      })
    }
  }

  def groundActs(world: StripsWorld): Unit = {
    val actions = world.actions
    val const: Array[String] = world.constants.toArray
    def unifyAll(z: Array[(String, String)],
                 predicates: Set[Predicate]): Set[Predicate] = {
      predicates.map(_.unify(z))
    }

    for (a <- actions) {
      val size = a.params.length
      val possible =
        ground(const, {for (_ <- 1 to size) yield {const(0)}}.toArray, size)
      possible.foreach( poss => {
        val z = a.params.zip(poss)
        val newAct = Action(
          a.name,
          poss,
          unifyAll(z, a.pre),
          unifyAll(z, a.preNeg),
          unifyAll(z, a.del),
          unifyAll(z, a.add))
        groundedActs = groundedActs + newAct
      })
    }
  }

  private def ground(a: Array[String],
             result: Array[String],
             size: Int,
             i: Int = 0,
             results: Array[Array[String]] = Array.empty
            ): Array[Array[String]] = {
    if (i == size) results :+ result
    else {
      var fResults: Array[Array[String]] = results
      for (j <- a.indices) {
        val newResult = ground(a, result.updated(i, a(j)), size, i+1)
        if (newResult.forall(p => p.toSet.size == p.length))
          fResults = fResults ++ newResult
      }
      fResults
    }
  }
}
