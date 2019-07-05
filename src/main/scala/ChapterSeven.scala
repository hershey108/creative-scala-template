import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

object Box {
  val aBox = Image.rectangle(20,20).fillColor(Color.royalBlue)

  val oneBox = aBox

  val twoBoxes = aBox beside oneBox

  val threeBoxes = aBox beside twoBoxes

  def boxes(count: Int): Image = {
    count match {
      case 0 => Image.empty
      case _ => aBox beside boxes(count-1)
    }
  }

  def stackedBoxes(count: Int): Image = {
    count match {
      case 0 => Image.empty
      case _ => aBox above stackedBoxes(count - 1)
    }
  }

  def brokenMatch(a: Int) = {
    a match {
      case 0 => "zero"
      case 1 => "one"
    }
  }

  def main(args: Array[String]): Unit = {
    boxes(4).draw

    stackedBoxes(6).draw

    brokenMatch(2)
  }

}

object Cross {
  val aCircle = Image.circle(10)

  def cross(count: Int): Image = {
    count match {
      case 0 => aCircle
      case n => aCircle above (aCircle beside cross(n-1) beside aCircle) above aCircle
    }
  }

  def main(args: Array[String]): Unit = {
    cross(4).draw
  }
}

object Chessboard {

  val redSquare = Image.rectangle(20,20).fillColor(Color.red)
  val blackSquare = Image.rectangle(20,20).fillColor(Color.black)

  val chessboardUnit = (redSquare beside blackSquare) above (blackSquare beside redSquare)

  def chessboard(count: Int): Image = {
    count match {
      case 0 => chessboardUnit
      case n =>
        val unit = chessboard(n-1)
        (unit beside unit) above (unit beside unit)
    }
  }

  def main(args: Array[String]): Unit = {
    chessboard(1).draw
    chessboard(2).draw
  }
}

object SierpinskiTriangle {
  def pinkTriangle = Image.triangle(10,11).lineColor(Color.hotpink)
  def base = pinkTriangle above (pinkTriangle beside pinkTriangle)

  def sierpinski(count: Int) : Image = {
    count match {
      case 0 => base
      case n =>
        val unit = sierpinski(n-1)
        unit above (unit beside unit)
    }
  }

  def main(args: Array[String]): Unit = {
    sierpinski(5).draw
  }
}