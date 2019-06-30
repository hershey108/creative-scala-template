import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

object Maths {

  def square(root: Int): Int = {
    root * root
  }

  def halve(double: Double) : Double = {
    double/2
  }

  def evaluateOrder(a: Int, b: Int) = {
    println("In the method")
    a + b
  }

  def main(args: Array[String]): Unit = {
    evaluateOrder({println("a"); 2}, {println("b"); 3})
  }

}