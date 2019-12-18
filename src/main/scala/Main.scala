import scala.io.{Source, StdIn}
import scala.util.{Success, Try}

object Day1 extends App {

  @scala.annotation.tailrec
  def calculateFuel(mass: Int, acc: Int = 0): Int =
    Math.max(mass / 3 - 2, 0) match {
      case 0 => acc
      case i => calculateFuel(i, acc + i)
    }

  val inputFile = Source.fromFile("input1")
  val result = inputFile.getLines().map(_.toIntOption).collect {
    case Some(i) => i
  }.map(calculateFuel(_)).sum
  println(result)
}

object Day2 extends App {

  def compute(program: Iterator[Int], p1: Int, p2: Int): Int = {
    val mutProgram = scala.collection.mutable.Seq.from(program)
    mutProgram.update(1, p1)
    mutProgram.update(2, p2)
    var thunk = 0
    var stop = false
    while (thunk * 4 < mutProgram.size && !stop) {
      val startIdx = thunk * 4
      val op = mutProgram(startIdx)
      val p1 = mutProgram(startIdx + 1)
      val p2 = mutProgram(startIdx + 2)
      val p3 = mutProgram(startIdx + 3)
      thunk += 1
      op match {
        case 1 =>
          mutProgram.update(p3, mutProgram(p1) + mutProgram(p2))
        case 2 =>
          mutProgram.update(p3, mutProgram(p1) * mutProgram(p2))
        case 99 =>
          stop = true
        case _ => throw new Exception("unknown operator")
      }
    }
    mutProgram.head
  }

  val inputFile = Source.fromFile("input2")
  val program = inputFile.getLines().flatMap(_.split(",").map(_.toInt)).toList
  (0 to program.size).foreach { i =>
    (0 to program.size).foreach { j =>
      if (Try(compute(program.iterator, i, j)) == Success(19690720)) {
        println(100 * i + j)
        System.exit(0)
      }
    }
  }
}

object Day3 extends App {

  case class Point(x: Int, y: Int, steps: Int) {
    def distant: Int = x.abs + y.abs
  }

  import scala.collection.mutable

  val pattern = "^([RLUD])(\\d+)$".r
  val dots = mutable.ListBuffer[mutable.ListBuffer[Point]]()
  val inputFile = Source.fromFile("input3")
  val program = inputFile.getLines().map(_.split(","))
  program.zipWithIndex.foreach { case (cmds, idx) =>
    val pos = mutable.ListBuffer[Point]()
    cmds.foreach { cmd =>
      val lastPos = pos.lastOption.getOrElse(Point(0, 0, 0))
      val newPos = cmd match {
        case pattern(op, dis) =>
          op match {
            case "R" =>
              (1 to dis.toInt).map(s => Point(lastPos.x + s, lastPos.y, lastPos.steps + s))
            case "L" =>
              (1 to dis.toInt).map(s => Point(lastPos.x - s, lastPos.y, lastPos.steps + s))
            case "U" =>
              (1 to dis.toInt).map(s => Point(lastPos.x, lastPos.y + s, lastPos.steps + s))
            case "D" =>
              (1 to dis.toInt).map(s => Point(lastPos.x, lastPos.y - s, lastPos.steps + s))
          }
      }
      pos ++= newPos
    }
    dots += pos
  }
  var shortest = Int.MaxValue
  dots(0).foreach { ps1 =>
    dots(1).foreach { ps2 =>
      if (ps1.x == ps2.x && ps1.y == ps2.y) {
        val dist = ps1.steps + ps2.steps
        shortest = Math.min(dist, shortest)
      }
    }
  }
  println(shortest)
}

object Day4 extends App {
  val input = 246515 to 739105

  def satisfy(i: Int): Boolean = {
    if (i.toString.length != 6) false
    else {
      val s = i.toString
      val r = s.foldLeft((-1, false, true, Map[Int, Int]())) { // (lastNumber, hasDup, isIncrease, dupOccurence)
        case (acc, c) =>
          val i = c.toString.toInt
          if (i == acc._1) {
            (i, true, acc._3, acc._4.updated(i, acc._4.getOrElse(i, 1) + 1))
          } else if (i > acc._1) {
            (i, acc._2, acc._3, acc._4)
          } else {
            (i, acc._2, false, acc._4)
          }
      }
      r._3 && r._4.values.exists(_ == 2)
    }
  }

  println(input.map(satisfy).filter(s => s).size)
}

object Day5 extends App {
  import util.control.Breaks._

  def compute(program: Iterator[Int]): Unit = {
    val mutProgram = scala.collection.mutable.Seq.from(program)
    var pointer = 0
    breakable {
      while (pointer < mutProgram.size) {
        val op = mutProgram(pointer)
        op%100 match {
          case 1 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            pointer += 1
            val p3 = mutProgram(pointer)
            val v1 = if(modP1 == 0) mutProgram(p1) else p1
            val v2 = if(modP2 == 0) mutProgram(p2) else p2
            mutProgram.update(p3, v1 + v2)
            pointer += 1
          case 2 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            pointer += 1
            val p3 = mutProgram(pointer)
            val v1 = if(modP1 == 0) mutProgram(p1) else p1
            val v2 = if(modP2 == 0) mutProgram(p2) else p2
            mutProgram.update(p3, v1 * v2)
            pointer += 1
          case 3 =>
            pointer += 1
            val p = mutProgram(pointer)
            println("input:")
            val v = StdIn.readInt()
            mutProgram.update(p, v)
            pointer += 1
          case 4 =>
            pointer += 1
            val p = mutProgram(pointer)
            println(mutProgram(p))
            pointer += 1
          case 5 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            val v1 = if(modP1 == 0) mutProgram(p1) else p1
            val v2 = if(modP2 == 0) mutProgram(p2) else p2
            if (v1 != 0)
              pointer = v2
            else pointer += 1
          case 6 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            val v1 = if(modP1 == 0) mutProgram(p1) else p1
            val v2 = if(modP2 == 0) mutProgram(p2) else p2
            if (v1 == 0) pointer = v2
            else pointer += 1
          case 7 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            pointer += 1
            val p3 = mutProgram(pointer)
            val v1 = if(modP1 == 0) mutProgram(p1) else p1
            val v2 = if(modP2 == 0) mutProgram(p2) else p2
            if(v1 < v2) mutProgram.update(p3, 1)
            else mutProgram.update(p3, 0)
            pointer += 1
          case 8 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            pointer += 1
            val p3 = mutProgram(pointer)
            val v1 = if(modP1 == 0) mutProgram(p1) else p1
            val v2 = if(modP2 == 0) mutProgram(p2) else p2
            if(v1 == v2) mutProgram.update(p3, 1)
            else mutProgram.update(p3, 0)
            pointer += 1
          case 99 =>
            break
        }
      }
    }
  }

  val inputFile = Source.fromFile("input5")
  val program = inputFile.getLines().flatMap(_.split(",").map(_.toInt))
  compute(program)
}