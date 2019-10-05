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
    def iter(count: Int, factor: Double) : List[Instruction] = {
      count match {
        case 0 => Nil
        case n => forward(factor) :: turn(89.degrees) :: iter(count -1, factor + 5)
      }
    }

    Turtle.draw(iter(turns, 10))
  }

  def main(args: Array[String]): Unit = {
    squareSpiral(120).draw
  }
}

// f, 85, f2, 85, f3, 85, n