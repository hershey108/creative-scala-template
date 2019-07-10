import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

object ConcentricShapes {
  def concentricShapes(count: Int, singleShape: Int => Image): Image = count match {
    case 0 => Image.empty
    case n => singleShape(n) on concentricShapes(n - 1, singleShape)
  }

  val circles = (n: Int) => {
    val color = Color.blue desaturate 0.5.normalized spin (n * 30).degrees
    val shape = Image.circle(50 + n * 12)
    shape lineWidth 10 lineColor color
  }

  val squares = (n: Int) => {
    val color = Color.blue desaturate 0.5.normalized spin (n * 30).degrees
    val shape = Image.rectangle(100 + n * 24, 100 + n * 24)
    shape lineWidth 10 lineColor color
  }

  val triangles = (n: Int) => {
    val color = Color.blue fadeOut (1 - n / 20.0).normalized
    val shape = Image.triangle(100 + n * 24, 100 + n * 24)
    shape lineWidth 10 lineColor color
  }


  def colored(shape: Int => Image, color: Int => Color): Int => Image = (n: Int) => {
    shape(n) lineColor color(n) lineWidth (10)
  }

  def fading(n: Int): Color = {
    Color.blue fadeOut (1 - n / 20.0).normalized
  }

  def spinning(n: Int): Color = {
    Color.blue spin (n * 30).degrees
  }

  def main(args: Array[String]): Unit = {
    concentricShapes(10, colored(circles, spinning)) beside
      concentricShapes(10, colored(triangles, fading)) beside
      concentricShapes(10, colored(squares, spinning)) draw
    //    (concentricShapes(6,circles) beside concentricShapes(6,triangles _) beside concentricShapes(6,squares _)).draw
  }
}

object Flower {
  def parametricCircle(angle: Angle): Point =
    Point.cartesian(angle.cos, angle.sin)

  def rose(angle: Angle) =
    Point.cartesian((angle * 7).cos * angle.cos, (angle * 7).cos * angle.sin)

  def weird(angle: Angle, scale: Int) =
    Point.cartesian((angle * scale).cos * angle.cos, (angle * scale).cos * angle.sin)

  def scale(factor: Double): Point => Point =
    (pt: Point) => Point.polar(pt.r * factor, pt.angle)

  def sample(start: Angle, samples: Int, location: Angle => Point): Image = {
    val step = Angle.one / samples
    val dot = triangle(10, 10)

    def loop(count: Int): Image = {
      val angle = step * count
      count match {
        case 0 => Image.empty
        case n => dot at location(angle).toVec on loop(n - 1)
      }
    }

    loop(samples)
  }

  def otherSample(start: Angle, samples: Int, location: (Angle, Int) => Point): Image = {
    val step = Angle.one / samples
    val dot = triangle(10, 10)

    def loop(count: Int): Image = {
      val angle = step * count
      count match {
        case 0 => Image.empty
        case n => dot at location(angle, n).toVec on loop(n - 1)
      }
    }

    loop(samples)
  }

  def locate(scale: Point => Point, point: Angle => Point): Angle => Point = (angle: Angle) => scale(point(angle))
  def otherLocate(scale: Point => Point, point: (Angle, Int) => Point): (Angle, Int) => Point = (angle: Angle, factor: Int) => scale(point(angle,factor))

  val flower = {
    sample(0.degrees, 200, locate(scale(200), rose _)) on
      sample(0.degrees, 40, locate(scale(150), parametricCircle _))
  }

  val other = {
    otherSample(0.degrees, 200, otherLocate(scale(200), weird _))
  }

  def main(args: Array[String]): Unit = {
    other.draw
  }
}