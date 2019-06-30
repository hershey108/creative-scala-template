import doodle.backend.StandardInterpreter._
import doodle.core.Image._
import doodle.core.Line.Cap.Square
import doodle.core.{Fill, _}
import doodle.jvm.Java2DFrame._
import doodle.syntax._

// To use this example:
//
// 1. run `sbt`
// 2. run the `console` command within `sbt`
// 3. enter `Mine.image.draw`
object MyStreet {

  val roof = triangle(50, 30) fillColor Color.red
  val house = {
    rectangle(50, 25) fillColor Color.red lineColor Color.red above {
      rectangle(15, 25) fillColor Color.black on
        rectangle(50,25) fillColor Color.red lineColor Color.red
    }
  }

  var garden = {
    circle(25) fillColor Color.green above rectangle(15,30) fillColor Color.brown
  }

  var road = {
    (rectangle(50,5) fillColor Color.yellow beside rectangle(50,5) fillColor Color.black) above
    rectangle(100,10) fillColor Color.black
  }

  val scene = ((roof above house) beside garden) above road

  val image = scene beside scene beside scene beside scene beside scene

  def main(args: Array[String]): Unit = {
    image.draw
  }
}




