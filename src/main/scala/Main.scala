import java.io.InputStream
import java.util.concurrent.{BlockingQueue, Executors, LinkedTransferQueue, SynchronousQueue, TimeUnit}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}

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

object Day6 extends App {
  import collection.mutable
  case class O(name: String, orbitedBy: mutable.ListBuffer[O]) {
    var orbits: Option[O] = None
  }
  val os = mutable.Map[String, O]()
  val inputs = Source.fromFile("input6").getLines()
  for (i <- inputs) {
    val x = i.split(')')
    val o1 = os.getOrElseUpdate(x(0), O(x(0), mutable.ListBuffer()))
    val o2 = os.getOrElseUpdate(x(1), O(x(1), mutable.ListBuffer()))
    o1.orbitedBy += o2
    o2.orbits = Some(o1)
  }
  def calculate(o: O, indirect: Int): Int = {
    o.orbitedBy.map(x => calculate(x, indirect + 1)).sum + 1 + indirect
  }
  val r = os("COM").orbitedBy.map(x => calculate(x, 0)).sum
  println(r)
  def path(o: O): List[String] = {
    o.orbits match {
      case Some(x)=> path(x) :+ o.name
      case None => List(o.name)
    }
  }
  val p1 = path(os("YOU"))
  val p2 = path(os("SAN"))
  val r2 = p1.size + p2.size - 2 * p1.intersect(p2).size - 2
  println(r2)
}

// not finished
object Day7 extends App {
  import util.control.Breaks._
  import scala.collection.mutable

  def compute(mutProgram: mutable.ListBuffer[Int])
             (input: SynchronousQueue[Int], output: SynchronousQueue[Int]): ListBuffer[Int] = {
    var pointer = 0
    val outputs: ListBuffer[Int] = mutable.ListBuffer[Int]()
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
            val v = input.take()
            mutProgram.update(p, v)
            pointer += 1
          case 4 =>
            pointer += 1
            val p = mutProgram(pointer)
            output.offer(mutProgram(p), 3, TimeUnit.SECONDS)
            outputs += mutProgram(p)
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

    outputs
  }

  import scala.concurrent.duration._
  val inputFile = Source.fromFile("input7")
  val pool = Executors.newFixedThreadPool(6)
  implicit val ec = ExecutionContext.fromExecutor(pool)
  val program = inputFile.getLines().flatMap(_.split(",").map(_.toInt)).toList
  val a = (5 to 9).permutations.toList
  def amplify(phazes: Seq[Int]): Int = {
    val amps = (1 to 5).map(_ => compute(mutable.ListBuffer.from(program))(_, _))
    // initialize the program with phase
    val inputOutput = (0 to 4).map{_ =>
      new SynchronousQueue[Int]()
    }
    val futures = amps.zipWithIndex.map { case (amp, i) =>
      val f = Future(amp(inputOutput(i), inputOutput((i + 1)%5)))
      f.onComplete {
        case Success(t) => t
        case Failure(e) => throw e
      }
      f
    }
    inputOutput.zipWithIndex.foreach { case(io, idx) =>
      io.put(phazes(idx))
    }
    inputOutput(0).put(0)
    val l = Await.result(futures.last, 1.minute)
    l.max
  }

  println(a.map(amplify).max)
//  println(amplify(List(9, 8, 7, 6, 5)))
}

object Day8 extends App {
  val input = Source.fromFile("input8").getLines().toList.head
  val width = 25
  val height = 6
  val layers = input.grouped(width * height).toList
  val minLayer = layers.map(l => (l.count(_ == '0'), l))
    .minBy(_._1)._2
  println(minLayer.count(_ == '1') * minLayer.count(_ == '2'))
  def merge(a: String, b: String): String = {
    a.zipWithIndex.map{
      case (c, idx) => c match {
        case '2' => b(idx)
        case _ => c
      }
    }.mkString
  }
  val image = layers.reduceLeft(merge).map(c => c match {
    case '0' => ' '
    case '1' => '#'
  })
  image.grouped(width).foreach(println)
}

object Day9 extends App {
  import util.control.Breaks._
  import scala.collection.mutable

  def compute(mutProgram: mutable.Map[BigInt, BigInt])
             (input: SynchronousQueue[BigInt]): ListBuffer[BigInt] = {
    var pointer: BigInt = 0
    var relativeBase: BigInt = 0
    val outputs: ListBuffer[BigInt] = mutable.ListBuffer[BigInt]()
    def getParamValue(param: BigInt, mode: BigInt): BigInt = {
      mode.toInt match {
        case 0 => mutProgram.getOrElseUpdate(param, 0)
        case 1 => param
        case 2 => mutProgram.getOrElseUpdate(relativeBase + param, 0)
      }
    }
    breakable {
      while (true) {
        val op = mutProgram(pointer)
        (op%100).toInt match {
          case 1 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            val modP3 = op % 100000 / 10000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            pointer += 1
            val p3 = mutProgram(pointer)
            val v1 = getParamValue(p1, modP1)
            val v2 = getParamValue(p2, modP2)
            val updatePosition = if (modP3 == 2) p3 + relativeBase else p3
            mutProgram.update(updatePosition, v1 + v2)
            pointer += 1
          case 2 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            val modP3 = op % 100000 / 10000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            pointer += 1
            val p3 = mutProgram(pointer)
            val v1 = getParamValue(p1, modP1)
            val v2 = getParamValue(p2, modP2)
            val updatePosition = if (modP3 == 2) p3 + relativeBase else p3
            mutProgram.update(updatePosition, v1 * v2)
            pointer += 1
          case 3 =>
            val modP1 = op % 1000 / 100
            pointer += 1
            val p = mutProgram(pointer)
            val i = input.take()
//            val v = getParamValue(p, modP1)
            val updatePosition = if (modP1 == 2) p + relativeBase else p
            mutProgram.update(updatePosition, i)
            pointer += 1
          case 4 =>
            val modP1 = op % 1000 / 100
            pointer += 1
            val p = mutProgram(pointer)
//            output.offer(mutProgram.getOrElseUpdate(p, 0), 3, TimeUnit.SECONDS)
            outputs += getParamValue(p, modP1)
//            outputs += mutProgram.getOrElseUpdate(p, 0)
            pointer += 1
          case 5 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            val v1 = getParamValue(p1, modP1)
            val v2 = getParamValue(p2, modP2)
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
            val v1 = getParamValue(p1, modP1)
            val v2 = getParamValue(p2, modP2)
            if (v1 == 0) pointer = v2
            else pointer += 1
          case 7 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            val modP3 = op % 100000 / 10000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            pointer += 1
            val p3 = mutProgram(pointer)
            val v1 = getParamValue(p1, modP1)
            val v2 = getParamValue(p2, modP2)
            val updatePosition = if (modP3 == 2) p3 + relativeBase else p3
            if(v1 < v2) mutProgram.update(updatePosition, 1)
            else mutProgram.update(updatePosition, 0)
            pointer += 1
          case 8 =>
            val modP1 = op % 1000 / 100
            val modP2 = op % 10000 / 1000
            val modP3 = op % 100000 / 10000
            pointer += 1
            val p1 = mutProgram(pointer)
            pointer += 1
            val p2 = mutProgram(pointer)
            pointer += 1
            val p3 = mutProgram(pointer)
            val v1 = getParamValue(p1, modP1)
            val v2 = getParamValue(p2, modP2)
            val updatePosition = if (modP3 == 2) p3 + relativeBase else p3
            if(v1 == v2) mutProgram.update(updatePosition, 1)
            else mutProgram.update(updatePosition, 0)
            pointer += 1
          case 9 =>
            val modP1 = op % 1000 / 100
            pointer += 1
            val p1 = mutProgram(pointer)
            relativeBase += getParamValue(p1, modP1)
            pointer += 1
          case 99 =>
            break
        }
      }
    }

    outputs
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  def toMutableMap(l: List[Int]) = {
    mutable.Map.from(l.zipWithIndex.map{
      case (v, idx) => (BigInt(idx), BigInt(v))
    })
  }
  val l1 = List(109, -1, 4, 1, 99)
  println(compute(toMutableMap(l1))(null))
  val l2 = List(109, -1, 104, 1, 99)
  println(compute(toMutableMap(l2))(null))
  val l3 = List(109, -1, 204, 1, 99)
  println(compute(toMutableMap(l3))(null))
  val l4 = List(109, 1, 9, 2, 204, -6, 99)
  println(compute(toMutableMap(l4))(null))
  val l5 = List(109, 1, 109, 9, 204, -6, 99)
  println(compute(toMutableMap(l5))(null))
  val l6 = List(109, 1, 209, -1, 204, -106, 99)
  println(compute(toMutableMap(l6))(null))
  val l7 = List(109, 1, 3, 3, 204, 2, 99)
  val i7 = new SynchronousQueue[BigInt]()
  val f1 = Future(compute(toMutableMap(l7))(i7))
  i7.put(10)
  println(Await.result(f1, 10.seconds))
  val l8= List(109, 1, 203, 2, 204, 2, 99)
  val i8 = new SynchronousQueue[BigInt]()
  val f2 = Future(compute(toMutableMap(l8))(i8))
  i8.put(111)
  println(Await.result(f2, 10.seconds))
  val source = Source.fromFile("input9").getLines().flatMap(_.split(',')).toList
  val memory = mutable.Map.from(source.zipWithIndex.map{
    case (v, idx) => (BigInt(idx), BigInt(v))
  })
  val input = new SynchronousQueue[BigInt]()
  val f = Future(compute(memory)(input))
  input.put(2)
  val result = Await.result(f, 1.minute)
  println(result)
}