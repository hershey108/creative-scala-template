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
  val pinkTriangle = {println("creating pink triangle"); Image.triangle(10,11).lineColor(Color.hotpink)}
  val base = {println("creating base"); pinkTriangle above (pinkTriangle beside pinkTriangle)}

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

object GradientBoxes {

  val aBox = Image.rectangle(200,200)

  def gradientBoxes(count: Int, colour: Color) : Image = {
    count match {
      case 0 => Image.empty
      case n => aBox.fillColor(colour).lineColor(colour.spin(Angle.degrees(15))).lineWidth(15) beside gradientBoxes(n - 1, colour.spin(Angle.degrees(30)))
    }
  }

  def main(args: Array[String]): Unit = {
    gradientBoxes(5, Color.royalBlue).draw
  }
}

object ConcentricCircles {

  def concentricCircles(count: Int, size: Int): Image = {
    count match {
      case 0 => Image.empty
      case n => Circle(size).lineColor(Color.royalBlue).lineWidth(10) on concentricCircles(n-1, size + 20 )
    }
  }

  def main(args: Array[String]): Unit = {
    concentricCircles(12, 60).draw
  }

}

object GradientConcentricCircles {

  def gradientConcentricCircles(count: Int, size: Int, colour: Color): Image = {
    count match {
      case 0 => Image.empty
      case n => Circle(size).lineColor(colour).lineWidth(10) on gradientConcentricCircles(n-1, size + 20, colour.spin((20).degrees).fadeOut(0.05.normalized))
    }
  }

  def main(args: Array[String]): Unit = {
    gradientConcentricCircles(12, 60, Color.royalBlue).draw
  }

}

object FixedChessboard {

  def chessboard(count: Int): Image = {
    val redSquare = Image.rectangle(20,20).fillColor(Color.red)
    val blackSquare = Image.rectangle(20,20).fillColor(Color.black)

    val chessboardUnit = (redSquare beside blackSquare) above (blackSquare beside redSquare)

    def loop(count: Int): Image = {
      count match {
        case 0 => chessboardUnit
        case n =>
          val unit = loop(n - 1)
          (unit beside unit) above (unit beside unit)
      }
    }
    loop(count)
  }

  def main(args: Array[String]): Unit = {
    chessboard(1).draw
    chessboard(2).draw
  }
}

object CleverBoxes {

  def boxes(count: Int): Image = {
    val aBox = Image.rectangle(100,100).fillColor(Color.royalBlue)

    def loop(count: Int): Image = {
      count match {
        case 0 => Image.empty
        case n => aBox beside loop(n-1)
      }
    }

    loop(count)
  }

  def main(args: Array[String]): Unit = {
    boxes(6).draw
  }
}