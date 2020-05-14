import scala.io.StdIn

object STRIPSParser {

  case class Predicate(name: String, variables: Array[String]) {
    override def toString: String = s"$name(" + variables.mkString(", ") + ")"

    override def hashCode(): Int = {
      name.hashCode + variables.deep.hashCode()
    }
    override def equals(obj: Any): Boolean = this.hashCode() == obj.hashCode()

    def unify(replace: Array[(String, String)]): Predicate = {
      var newVars: Array[String] = variables.indices.map(_ => "").toArray
      replace.foreach({ r =>
        val i = variables.indexOf(r._1)
        if (i != -1) newVars = newVars.updated(i, r._2)
      })
      Predicate(name, newVars)
    }
  }

  case class Action( name: String,
                     params: Array[String],
                     pre: Set[Predicate],
                     preNeg: Set[Predicate],
                     del: Set[Predicate],
                     add: Set[Predicate]
                   ) {
    override def toString: String = s"$name " + params.mkString(" ")
  }

  def parse(): StripsWorld = {
    val predList = parsePredList("predicates:")
    var constants = parseLn("constants:")
    val numActions = nextLine().split(" ") match {
      case Array(h, "actions") => h.toInt
      case _ => throw new Exception("Expected action list size.")
    }

    val actions = for (_ <- 1 to numActions) yield {
      val action = nextLine().split(" ")
//      println(action.head)/////////////////////////////////////////////////////////
      Action(
        action.head,
        action.tail,
        parsePredList("pre:").toSet,
        parsePredList("preneg:").toSet,
        parsePredList("del:").toSet,
        parsePredList("add:").toSet
      )
//      println(act)
    }

    constants = constants ++ parseLn("constants:")
    val initial = parsePredList("initial:")
    val goal = parsePredList("goal:")
    val goalNeg = parsePredList("goalneg:")

    new StripsWorld(
      predList.toSet,
      constants.toSet,
      actions.toSet,
      initial.toSet,
      goal.toSet,
      goalNeg.toSet
    )

  }

  def parsePredList(des: String): Array[Predicate] =
    parseLn(des).map(
      {p =>
//        println(s">>>$p<<<<")
      parsePred(p, 0)._1}
    )

  def parseLn(des: String): Array[String] = {
    val line: Array[String] =
      nextLine().replaceAll(" (?=(\\w, )*\\w*\\))", "").split(" ")
//    println("}}}}}}")
//    line.foreach(x => print(s" $x "))
//    println("{{{{{")
    line match {
      case Array(h, _*) if h == des => line.tail
      case Array(h, _*) => throw new Exception(
        s"Unexpected line designation - Expected: '$des', Actual: '$h'"
      )
    }
  }

  def parsePred(s: String, i: Int): (Predicate, Int) = {
    var n = i
    var end = nameEnd(s, i)
    val name = s.slice(i, end)
    n = end
    end = s.indexOf(')')
    val vars = s.slice(n+1, end).replaceAll("\\s", "").split(",")
//    vars.foreach(println(_))
    (Predicate(name, vars), end+1)
  }

  def nameEnd(s: String, i: Int): Int =
    s.indexWhere(!_.isLetterOrDigit, i) match {
      case -1 => i + s.length
      case n => n
    }

  def next(s: String, i: Int): Int = s.indexWhere( !_.isWhitespace, i )

  def nextLine(): String = {
    val line = StdIn.readLine()
    if (line.length == 0 || line.charAt(0) == '#') nextLine()
    else line
  }

  def main(args: Array[String]): Unit = {
    parse()
  }
}
