import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._
import doodle.core.Point._
import doodle.core.PathElement._
import doodle.examples.CreativeScala.stars

object Polygons{

  val triangle =
    closedPath(List(
      moveTo(polar(50,0.degrees)),
      lineTo(polar(50,120.degrees)),
      lineTo(polar(50,240.degrees))
    ))

  val square =
    closedPath(List(
      moveTo(polar(50,45.degrees)),
      lineTo(polar(50,135.degrees)),
      lineTo(polar(50,225.degrees)),
      lineTo(polar(50,315.degrees))
    ))

  val angle = 360/5
  val startAngle = 90 - angle

  val pentagon =
    closedPath(List(
      moveTo(polar(50,0.degrees)),
      lineTo(polar(50,((1*angle)).degrees)),
      lineTo(polar(50,((2*angle)).degrees)),
      lineTo(polar(50,((3*angle)).degrees)),
      lineTo(polar(50,((4*angle)).degrees)),
    ))

  val spacer =
    rectangle(10, 100).noLine.noFill
  def style(image: Image): Image = image.lineWidth(6.0).lineColor(Color.paleTurquoise).fillColor(Color.turquoise)


  def main(args: Array[String]): Unit = {
    (style(triangle) beside spacer beside style(square) beside spacer beside style(pentagon)).draw
  }
}

object Curves {

  def curve(radius: Int, start: Angle, increment: Angle): PathElement = {
    curveTo(
      polar(radius * 0.7, start + increment*0.2),
      polar(radius * 1.4, start + increment*0.7),
      polar(radius, start+increment)
    )
  }

  val triangle =
    closedPath(List(
      moveTo(polar(50,0.degrees)),
      curve(50, 0.degrees, 120.degrees),
      curve(50, 120.degrees, 120.degrees),
      curve(50, 240.degrees, 120.degrees)
    ))

  val square =
    closedPath(List(
      moveTo(polar(50,45.degrees)),
      curve(50, 45.degrees,90.degrees),
      curve(50, 135.degrees,90.degrees),
      curve(50, 225.degrees,90.degrees),
      curve(50, 315.degrees,90.degrees),
    ))

  val angle = 360/5
  val startAngle = 90 - angle

  val pentagon =
    closedPath(List(
      moveTo(polar(50,0.degrees)),
      curve(50, 0.degrees, angle.degrees),
      curve(50, (1*angle).degrees, angle.degrees),
      curve(50, (2*angle).degrees, angle.degrees),
      curve(50, (3*angle).degrees, angle.degrees),
      curve(50, (4*angle).degrees, angle.degrees),
    ))


  val spacer =
    rectangle(10, 100).noLine.noFill
  def style(image: Image): Image = image.lineWidth(6.0).lineColor(Color.paleTurquoise).fillColor(Color.turquoise)


  def main(args: Array[String]): Unit = {
    (style(triangle) beside spacer beside style(square) beside spacer beside style(pentagon)).draw
  }

}

object BuildingLists {
  def ones(count: Int): List[Int] = {
    count match {
      case 0 => Nil
      case n => 1 :: ones(n-1)
    }
  }

  def descending(n: Int): List[Int] = {
    n match {
      case 0 => Nil
      case x => x :: descending(x - 1)
    }
  }

  def ascending(n: Int): List[Int] = {
    def iter(count: Int, counter: Int): List[Int] = {
      count match {
        case 0 => Nil
        case x => counter :: iter(x-1, counter+1)
      }
    }
    iter(n, 1)
  }

  def fill[A](count: Int, a: A): List[A] = {
    count match {
      case 0 => Nil
      case x => a :: fill(x-1, a)
    }
  }

  def main(args: Array[String]): Unit = {
    println(fill(5, "banana"))
  }
}

object TransformingLists {
  def double(l: List[Int]): List[Int] = {
    l match {
      case Nil => Nil
      case hd :: tl => hd*2 :: double(tl)
    }
  }

  def product(l: List[Int]): Int = {
    l match {
      case Nil => 1
      case hd :: tl => hd * product(tl)
    }
  }

  def contains[A](l: List[A], a: A): Boolean = {
    if (a == Nil) {
      true
    } else {

      l match {
        case Nil => false
        case hd :: tl => (hd == a) || contains(tl, a)
      }
    }
  }

  def first[A](l: List[A], a: A): A = {
    l match {
      case Nil => a
      case hd :: tl => hd
    }
  }

  def reverse[A](l: List[A]): List[A] = {
    def iter(k: List[A], answer: List[A]): List[A] = {
      k match {
        case Nil => answer
        case hd :: tl => iter(tl, hd :: answer)
      }
    }
    iter(l, Nil)
  }

  def main(args: Array[String]): Unit = {
    println(first(List(1,4,3),3))
    println(first(Nil,3))

    println(reverse(List(1,4,3)))
    println(reverse(Nil))
  }
}

object SmartPolygons {
  def polygon(sides: Int, startingRotation: Angle, size: Double): Image = {

    def liner(count: Int, startingAngle: Angle): List[PathElement] = {
      def iter(x: Int, sa: Angle, i: Angle, l: List[PathElement]): List[PathElement] = {
        x match {
          case 0 => l
          case n => lineTo(size, sa) :: iter(x - 1, sa + i, i, l)
        }
      }

      moveTo(polar(size, startingRotation)) :: iter(sides, startingRotation, (360/sides).degrees, Nil)

    }
    val lines = liner(sides-1, startingRotation)

    closedPath(lines)

  }

  def style(image: Image, spin: Double): Image = image.lineWidth(6.0).lineColor(Color.paleTurquoise.spin(spin.degrees)).fillColor(Color.turquoise.spin(spin.degrees))

  def shapes(count: Int, size: Double): Image = {
    def iter(count: Int, counter: Int, size: Double): Image = {
      count match {
        case 2 => Image.empty
        case n => style(polygon(counter, 0.degrees, size), size) on iter(n-1, counter + 1, size*1.2)
      }
    }

    iter(count, 3, size)
  }

  def main(args: Array[String]): Unit = {
    shapes(9, 80).draw
  }
}

object RangesListsMaps {
  def ones(i: Int): List[Int] = {
    (0 until i).map(x => 1).toList
  }

  def descending(i: Int): List[Int] = {
    i match {
      case 0 => Nil
      case n => (n until 0 by -1).map (x => x).toList
    }
  }

  def double(l: List[Int]): List[Int] = {
    l map(x => x*2)
  }

  def main(args: Array[String]): Unit = {
    println(ones(4))

    println(descending(0))
    println(descending(3))

    println(double(List(1,2,3)))
    println(double(List(4,9,16)))
  }
}

object PolygonsAgain {
  def polygon(sides: Int, startingRotation: Angle, size: Double): Image = {

    def liner(count: Int, startingAngle: Angle): List[PathElement] = {
      val increment = (Angle.one/sides).toDegrees.toInt
      (0 to 360 by increment).map(x => lineTo(size,x.degrees)).toList
    }
    val lines = liner(sides-1, startingRotation)

    closedPath(moveTo(polar(size, startingRotation)) :: lines)

  }

  def style(image: Image, spin: Double): Image = image.lineWidth(6.0).lineColor(Color.paleTurquoise.spin(spin.degrees)).fillColor(Color.turquoise.spin(spin.degrees))

  def shapes(count: Int, size: Double): Image = {
    def iter(count: Int, counter: Int, size: Double): Image = {
      count match {
        case 2 => Image.empty
        case n => style(polygon(counter, 0.degrees, size), size) on iter(n-1, counter + 1, size*1.2)
      }
    }

    iter(count, 3, size)
  }

  def main(args: Array[String]): Unit = {
    shapes(9, 80).draw
  }
}

object ToolsWithRanges {
  def ascending(n: Int): List[Int] = {
    (1 to n).toList
  }

  def main(args: Array[String]): Unit = {
    println(ascending(0))
    println(ascending(3))

  }
}

object Stars {

  def star(p: Int, n: Int, radius: Double): Image = {
    val increment = ((360.degrees * n) / p)
    val lines = (1 until p).map(x => lineTo(polar(radius, increment * x))).toList
    closedPath(moveTo(polar(radius, 0.degrees)) :: lines)
  }

  def style(img: Image, hue: Angle): Image = {
    img.
      lineColor(Color.hsl(hue, 1.normalized, .25.normalized)).
      fillColor(Color.hsl(hue, 1.normalized, .75.normalized))
  }

  def allBeside(images:List[Image]): Image = {
    images match {
      case Nil => Image.empty
      case hd :: tl => hd beside allBeside(tl)
    }
  }

  def allAbove(images:List[Image]): Image = {
    images match {
      case Nil => Image.empty
      case hd :: tl => hd above allAbove(tl)
    }
  }

  def main(args: Array[String]): Unit = {
    allAbove((3 to 33 by 2).map(p => {
      allBeside((1 to p/2).map(n =>{
        style(star(p, n, 20),360.degrees * p / n)
      }).toList)
    }).toList).draw
  }
}