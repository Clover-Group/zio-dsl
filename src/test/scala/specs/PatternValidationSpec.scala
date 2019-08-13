/* package patterns

import org.specs2._
import zio.{ DefaultRuntime }
// import TestEvents._

class ArrowSpec extends Specification with DefaultRuntime {

  val patterns = Seq(
    "doubleSensor1 > doubleSensor2",
    "intSensor + longSensor > 100",
    "boolSensor = true and lag(intSensor) = 5"
  )

  val fieldsTypes = Map(
    "doubleSensor1" -> "float64",
    "doubleSensor2" -> "float64",
    "floatSensor1"  -> "float32",
    "floatSensor2"  -> "float32",
    "longSensor"    -> "int64",
    "intSensor"     -> "int32",
    "shortSensor"   -> "int16",
    "byteSensor"    -> "int8",
    "boolSensor"    -> "boolean",
    "stringSensor"  -> "string",
    "anySensor"     -> "any"
  )

  def is = s2"""

  DSL Pattern Validator should
    validate patterns   $testPatternValidator

    """

  def testPatternValidator =
    // val results = PatternsValidator
    //   .validate[TestEvent](patterns.zipWithIndex.map(pi => RawPattern(pi._2.toString, pi._1)), fieldsTypes)
    true === true

}
 */
