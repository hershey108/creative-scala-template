import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

// To use this example:
//
// 1. run `sbt`
// 2. run the `console` command within `sbt`
// 3. enter `Mine.image.draw`
object Mine {
  val boss = circle(30) fillColor Color.red on circle(60) fillColor Color.white on circle(90) fillColor Color.red
  val stand = rectangle(10,40) above rectangle(40,10) fillColor Color.brown
  val field = rectangle(10,40) above rectangle(40,10) fillColor Color.brown

  val target = boss above stand above field
  def main(args: Array[String]): Unit = {
    target.draw
  }
}




