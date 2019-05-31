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
object Mine {

  def main(args: Array[String]): Unit = {
    ((circle(30) fillColor Color.red on circle(60) fillColor Color.white on circle(90) fillColor Color.red) above rectangle(10,40) above rectangle(40,10) fillColor Color.brown above rectangle(500, 10) fillColor Color.green).draw
  }
}




