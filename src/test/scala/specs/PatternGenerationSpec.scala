/* package patterns
import org.specs2._

import scala.reflect.ClassTag

import dsl._

class PatternGenerationSpec extends mutable.Specification {

  // import TestEvents._

  val fieldsClasses = Map(
    'intSensor     -> ClassTag.Int,
    'longSensor    -> ClassTag.Long,
    'boolSensor    -> ClassTag.Boolean,
    'doubleSensor1 -> ClassTag.Double,
    'doubleSensor2 -> ClassTag.Double
  )

  val gen = new ASTPatternGenerator[TestEvent, Symbol, Any]

  "Specification for Pattern Generator testing" >> {

    "Build valid patterns" >> {
      gen.build("doubleSensor1 > 0 for 30 sec", 0.0, fieldsClasses).right.value shouldBe a[(_, PatternMetadata)]
      gen.build("doubleSensor1 > 0 or longSensor = 0", 0.0, fieldsClasses).right.value shouldBe a[(_, PatternMetadata)]
      gen.build("doubleSensor1 > 0 and intSensor = 0", 0.0, fieldsClasses).right.value shouldBe a[(_, PatternMetadata)]
      gen
        .build("(doubleSensor1 + intSensor as float64) >= 10", 0.0, fieldsClasses)
        .right
        .value shouldBe a[(_, PatternMetadata)]
      gen
        .build("(doubleSensor1 + intSensor as float64) >= 10", 0.0, fieldsClasses)
        .right
        .value shouldBe a[(_, PatternMetadata)]
      gen
        .build("avgOf(doubleSensor1, doubleSensor2) >= 10 for 5 min >= 100 ms", 0.0, fieldsClasses)
        .right
        .value shouldBe a[(_, PatternMetadata)]
      gen
        .build("sin(doubleSensor1) >= 0.5 for 5 min andThen intSensor > 42", 0.0, fieldsClasses)
        .right
        .value shouldBe a[(_, PatternMetadata)]
      gen
        .build("tan(doubleSensor1) >= 1 for 5 hr andThen avg(doubleSensor2, 3 sec) > 42", 0.0, fieldsClasses)
        .right
        .value shouldBe a[(_, PatternMetadata)]
      gen
        .build("count(doubleSensor1, 4 sec) * sum(doubleSensor2, 3 sec) < 9", 0.0, fieldsClasses)
        .right
        .value shouldBe a[(_, PatternMetadata)]
      gen
        .build("lag(doubleSensor1, 10 sec) > doubleSensor1", 0.0, fieldsClasses)
        .right
        .value shouldBe a[(_, PatternMetadata)]
      //gen.build("boolSensor = true andThen boolSensor != false", 0.0, fieldsClasses).right.value shouldBe a[(Pattern[TestEvent, _, _], PatternMetadata)]
    }

    "Not generate invalid patterns" >> {
      gen.build("boolSensor > 0 for 30 sec", 0.0, fieldsClasses).left.value shouldBe a[Throwable]
    }

    // "Not buid invalid patterns " >> {
    // // Assert argument must be boolean
    //   a[RuntimeException] should be thrownBy gen.generatePattern(Assert(Constant(1.0)))
    // }

    "Perform casts correctly" >> {
      gen.build("doubleSensor1 as int32 > 0", 0.0, fieldsClasses).right.value shouldBe a[(_, PatternMetadata)]
      gen.build("doubleSensor1 as int64 > 0", 0.0, fieldsClasses).right.value shouldBe a[(_, PatternMetadata)]
      gen.build("doubleSensor1 as boolean = true", 0.0, fieldsClasses).right.value shouldBe a[(_, PatternMetadata)]

    }

  }
}
 */
