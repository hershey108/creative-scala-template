import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._
import doodle.turtle._
import doodle.turtle.Instruction._

object TurtlePolygons {
  def polygon(sides: Int, sideLength: Double): Image = {
    val shapeAngle = (360 / sides).degrees

    def loop(count: Int): List[Instruction] = {
      count match {
        case 0 => Nil
        case n => forward(sideLength) :: turn(shapeAngle) :: loop(count - 1)
      }
    }

    Turtle.draw(loop(sides))
  }

  def main(args: Array[String]): Unit = {
    polygon(6, 100).draw
  }
}

object SquareSpiral {
  def squareSpiral(turns: Int): Image = {
    def iter(count: Int, factor: Double): List[Instruction] = {
      count match {
        case 0 => Nil
        case n =>
          forward(factor) :: turn(89.degrees) :: iter(count - 1, factor + 5)
      }
    }

    Turtle.draw(iter(turns, 10))
  }

  def main(args: Array[String]): Unit = {
    squareSpiral(120).draw
  }
}

object SpokeBranchTurtle {
  def branching(spokes: Int): Image = {
    var instructions: List[Instruction] = Nil
    val angle = Angle.one / spokes

    (1 to spokes) foreach (
      x => instructions = turn(angle) :: branch(forward(100)) :: instructions
    )

    Turtle.draw(instructions)
  }

  def main(args: Array[String]): Unit = {
    branching(12).draw
  }
}

object Flatmapping {
  def double[A](l: List[A]): List[A] = {
    l flatMap { x =>
      List(x, x)
    }
  }

  def nothing[A](l: List[A]): List[A] = List.empty[A]

  def main(args: Array[String]): Unit = {
    println(double(List(1, 2, 3)))
    println(double(List("banana", "apple", "kumquat")))
    println(nothing(List(1, 2, 3)))
    println(nothing(List("do", "ray", "me")))
  }
}


object LSystem {
  val stepSize = 10
  // stepSize: Int = 10
  def rule(i: Instruction): List[Instruction] =
    i match {
      case Forward(_) => List(forward(stepSize), forward(stepSize)) case NoOp =>
        List(branch(turn(45.degrees), forward(stepSize), noop), branch(turn(-45.degrees), forward(stepSize), noop))
      case other => List(other)
    }


  def rewrite(instructions: List[Instruction],
              rule: Instruction => List[Instruction]): List[Instruction] = {
    instructions.flatMap {
      case Branch(instruction) => List(branch(rewrite(instruction, rule): _*))
      case other               => rule(other)
    }
  }

  def iterate(steps: Int,
              seed: List[Instruction],
              rule: Instruction => List[Instruction]): List[Instruction] = {
    steps match {
      case 0 => seed
      case n => iterate(n-1, rewrite(seed, rule), rule)
    }
  }

  def simpleBranching(steps: Int) = {
    iterate(steps,
      List(forward(100), branch(turn(45.degrees), forward(50), noop),branch(turn((-45).degrees), forward(50), noop)),
      rule)
  }

  def main(args: Array[String]): Unit = {
    Turtle.draw(simpleBranching(3)).draw
  }
}

object TurtlePolygon {
  def polygon(sides: Int, sideLength: Double): Image = {
    val angle = Angle.one / sides

    val instructions : List[Instruction] = (1 to sides).flatMap(_ => List(forward(sideLength), turn(angle))).toList

    Turtle.draw(instructions)
  }

  def squareSpiral(steps: Int, distance: Double, angle: Angle, increment: Double): Image = {
    Turtle.draw((1 to steps).toList.flatMap{ x => List(forward(distance+x*increment), turn(angle))})
  }

  def main(args: Array[String]): Unit = {
    polygon(18,100).draw

    squareSpiral(90,30,89.degrees,12).draw
  }
}
