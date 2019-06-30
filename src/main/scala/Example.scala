import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

// To use this example:
//
// 1. run `sbt`
// 2. run the `console` command within `sbt`
// 3. enter `Example.image.draw`
object Example {

  (circle(100) fillColor Color.paleGoldenrod lineColor Color.indianRed).draw
  val image = circle(10).fillColor(Color.red) on circle(20) on circle(30)

  def main(args: Array[String]): Unit = {
    image.draw
  }
}
