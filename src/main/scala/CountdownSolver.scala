import Operations.Operation

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Queue}

/**
 * Solves 'Countdown' tv quiz type puzzles.
 */
object CountdownSolver extends App {
  val solution = new RootNode(List(25, 100, 8, 10, 4, 5)).solve(507)
  println(if (solution.isEmpty) "NO SOLUTION" else solution.mkString("\n"))
}

/**
 * Four common arithmetic operations.
 */
object Operations extends Enumeration {
  type Operation = Value
  val NoOp, +, -, *, / = Value

  def apply(op: Operation, o1: Int, o2: Int): Option[Int] = {
    op match {
      case NoOp => None
      case + => Some(o1 + o2)
      case - => Some(o1 - o2)
      case * => Some(o1 * o2)
      case / => if (o2 != 0 && o1 % o2 == 0) Some(o1 / o2) else None
    }
  }

  def all: Seq[Operation] = List(+, -, *, /)
}

/**
 * One node of the solution tree.
 *
 * @param availableNumbers Remaining available numbers from which the expression can be composed.
 * @param value            Value of the node.
 * @param parent           Parent node in the solution tree.
 * @param level            Distance from the root in the solution tree.
 * @param operation        Arithmetic operation used to create the value.
 * @param o1               First operand.
 * @param o2               Second operand.
 */
case class Node(availableNumbers: Array[Int], value: Int, parent: Node, level: Int, operation: Operation, o1: Int, o2: Int) {

  override def toString: String = {
    if (parent == null) "" else {
      parent.toString() + s"$o1 $operation $o2 == $value, "
    }
  }
}

/**
 * Root node in the solution tree.
 *
 * @param availableNumbers Remaining available numbers from which the expression can be composed.
 */
class RootNode(availableNumbers: Seq[Int]) extends Node(availableNumbers.toArray, 0, null, 0, Operations.NoOp, -1, -1) {

  /**
   * Solve the puzzle. Return the solution.
   *
   * @param goal Puzzle goal.
   * @return
   */
  def solve(goal: Int): List[Node] = {
    val queue = mutable.Queue[Node]()
    queue.enqueue(this)
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      val solution = expandNodesAndCheckSolution(node, goal, queue)
      if (solution.nonEmpty) return solution
    }
    Nil
  }


  private def editArray(input: Array[Int], removed1: Int, removed2: Int, newValue: Int): Array[Int] = {
    val a = new ArrayBuffer[Int]()
    for (i <- input.indices if i != removed1 && i != removed2) a += input(i)
    a += newValue
    a.toArray
  }

  private def expandNodesAndCheckSolution(node: Node, goal: Int, queue: mutable.Queue[Node]): List[Node] = {
    var solutions: List[Node] = Nil
    val l = node.availableNumbers.length
    if (l < 2) return Nil
    for (i1 <- 0 until l) {
      for (i2 <- 0 until l if i1 != i2) {
        for (op <- Operations.all) {
          val o1 = node.availableNumbers(i1)
          val o2 = node.availableNumbers(i2)
          val value = Operations(op, o1, o2)
          if (value.isDefined) {
            val newNode = Node(editArray(node.availableNumbers, i1, i2, value.get),
              value.get, node, node.level + 1, op, o1, o2)

            if (value.get == goal) {
              solutions = newNode :: solutions
            } else {
              queue.enqueue(newNode)
            }
          }
        }
      }
    }
    solutions
  }
}

