package dsl
import org.specs2._

import scala.reflect.ClassTag

class ASTTest extends mutable.Specification {
  implicit val funReg = DefaultFunctionRegistry

  "Specification for AST testing" >> {
    "AST types must correctly construct from Scala types" >> {
      ASTType.of[Int] must be(IntASTType)
      ASTType.of[java.lang.Integer] must be(IntASTType)
      ASTType.of[Long] must be(LongASTType)
      ASTType.of[java.lang.Long] must be(LongASTType)
      ASTType.of[Boolean] must be(BooleanASTType)
      ASTType.of[java.lang.Boolean] must be(BooleanASTType)
      ASTType.of[Double] must be(DoubleASTType)
      ASTType.of[java.lang.Double] must be(DoubleASTType)
      ASTType.of[String] must be(StringASTType)
      ASTType.of[List[Int]] must be(AnyASTType)
    }

    "AST types must correctly determine" >> {
      Constant(1.0).valueType must be(DoubleASTType)
      Constant(1L).valueType must be(LongASTType)
      Constant(true).valueType must be(BooleanASTType)
      Constant(List(1, 2, 3)).valueType must be(AnyASTType)
    }

    "Identifiers must have correct types" >> {
      Identifier('intVar, ClassTag.Int).valueType must be(IntASTType)
      Identifier('longVar, ClassTag.Long).valueType must be(LongASTType)
      Identifier('boolVar, ClassTag.Boolean).valueType must be(BooleanASTType)
      Identifier('doubleVar, ClassTag.Double).valueType must be(DoubleASTType)
      Identifier('stringVar, ClassTag(classOf[String])).valueType must be(StringASTType)
    }

    "AST operations must require types" >> {
      FunctionCall('and, Seq(Constant(true), Constant(false))).valueType must be(BooleanASTType)
      FunctionCall('and, Seq(Constant(true))) must throwA[ParseException]              // only 1 argument
      FunctionCall('and, Seq(Constant(true), Constant(1))) must throwA[ParseException] // invalid types
    }

    "Reducers must control types" >> {
      ReducerFunctionCall('sumof, (_ => true), Seq(Constant(5.0), Constant(8.0))).valueType must be(DoubleASTType)
      ReducerFunctionCall('sumof, (_ => true), Seq(Constant(true), Constant(13.0))) must throwA[ParseException] // invalid types
      ReducerFunctionCall('samof, (_ => true), Seq(Constant(5.0), Constant(8.0))) must throwA[ParseException]   // invalid reducername
    }

    "AndThen must control types" >> {
      AndThen(Constant(true), Constant(true)).valueType must be(BooleanASTType)
      AndThen(Constant(true), Constant(1.0)).valueType must throwA[ParseException] // non-boolean second argument
      AndThen(Constant(1.0), Constant(true)).valueType must throwA[ParseException] // non-boolean first argument
    }

    "Windowed operators must construct correctly" >> {
      val winOp = ForWithInterval(
        FunctionCall('gt, Seq(Identifier('sensor, ClassTag.Double), Constant(0))),
        Some(false),
        Window(60000),
        TimeInterval(0, 10000)
      )
      winOp.valueType must be(BooleanASTType)
      winOp.metadata mustEqual (PatternMetadata(Set('sensor), 60000))
    }

    "Type requirements must be met" >> {
      Constant(10L).requireType(LongASTType, "Type was not long")
      Constant(10L).requireType(BooleanASTType, "Type was not boolean") must throwA[ParseException]
    }

    "Aggregate functions must be correctly created from symbols" >> {
      AggregateFn.fromSymbol('sum) must beRight(Sum)
      AggregateFn.fromSymbol('avg) must beRight(Avg)
      AggregateFn.fromSymbol('count) must beRight(Count)
      AggregateFn.fromSymbol('lag) must beRight(Lag)
      AggregateFn.fromSymbol('invalid) must beLeft
    }

    "Aggregate calls must correctly determine window sizes" >> {
      AggregateCall(Sum, Constant(1), Window(10000)).metadata.sumWindowsMs mustEqual 10000L
      AggregateCall(Sum, AggregateCall(Avg, Constant(1.0), Window(5000)), Window(10000)).metadata.sumWindowsMs mustEqual 15000L
    }

    "Range must be correctly constructed" >> {
      val range = Range(10000L, 60000L)
      range.metadata must be(PatternMetadata.empty)
      range.valueType must be(LongASTType)
    }
  }
}
