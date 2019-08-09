package dsl
import org.specs2._

import scala.reflect.ClassTag

class ASTTest extends mutable.Specification {
  implicit val funReg = DefaultFunctionRegistry

  "Specification for AST testing" >> {
    "AST types must correctly construct from Scala types" >> {
      ASTType.of[Int] === (IntASTType)
      ASTType.of[java.lang.Integer] === (IntASTType)
      ASTType.of[Long] === (LongASTType)
      ASTType.of[java.lang.Long] === (LongASTType)
      ASTType.of[Boolean] === (BooleanASTType)
      ASTType.of[java.lang.Boolean] === (BooleanASTType)
      ASTType.of[Double] === (DoubleASTType)
      ASTType.of[java.lang.Double] === (DoubleASTType)
      ASTType.of[String] === (StringASTType)
      ASTType.of[List[Int]] === (AnyASTType)
    }

    "AST types must correctly determine" >> {
      Constant(1.0).valueType === (DoubleASTType)
      Constant(1L).valueType === (LongASTType)
      Constant(true).valueType === (BooleanASTType)
      Constant(List(1, 2, 3)).valueType === (AnyASTType)
    }

    "Identifiers must have correct types" >> {
      Identifier('intVar, ClassTag.Int).valueType === (IntASTType)
      Identifier('longVar, ClassTag.Long).valueType === (LongASTType)
      Identifier('boolVar, ClassTag.Boolean).valueType === (BooleanASTType)
      Identifier('doubleVar, ClassTag.Double).valueType === (DoubleASTType)
      Identifier('stringVar, ClassTag(classOf[String])).valueType === (StringASTType)
    }

    "AST operations must require types" >> {
      FunctionCall('and, Seq(Constant(true), Constant(false))).valueType === (BooleanASTType)
      FunctionCall('and, Seq(Constant(true))) must throwA[ParseException]              // only 1 argument
      FunctionCall('and, Seq(Constant(true), Constant(1))) must throwA[ParseException] // invalid types
    }

    "Reducers must control types" >> {
      ReducerFunctionCall('sumof, (_ => true), Seq(Constant(5.0), Constant(8.0))).valueType === (DoubleASTType)
      ReducerFunctionCall('sumof, (_ => true), Seq(Constant(true), Constant(13.0))) must throwA[ParseException] // invalid types
      ReducerFunctionCall('samof, (_ => true), Seq(Constant(5.0), Constant(8.0))) must throwA[ParseException]   // invalid reducername
    }

    "AndThen must control types" >> {
      AndThen(Constant(true), Constant(true)).valueType === (BooleanASTType)
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
      winOp.valueType === (BooleanASTType)
      winOp.metadata mustEqual (PatternMetadata(Set('sensor), 60000))
    }

    "Type requirements === met" >> {
      Constant(10L).requireType(LongASTType, "Type was not long")
      Constant(10L).requireType(BooleanASTType, "Type was not boolean") must throwA[ParseException]
    }

    "Aggregate functions === correctly created from symbols" >> {
      AggregateFn.fromSymbol('sum) === Right(Sum)
      AggregateFn.fromSymbol('avg) === Right(Avg)
      AggregateFn.fromSymbol('count) === Right(Count)
      AggregateFn.fromSymbol('lag) === Right(Lag)
      AggregateFn.fromSymbol('invalid) must beLeft
    }

    "Aggregate calls must correctly determine window sizes" >> {
      AggregateCall(Sum, Constant(1), Window(10000)).metadata.sumWindowsMs mustEqual 10000L
      AggregateCall(Sum, AggregateCall(Avg, Constant(1.0), Window(5000)), Window(10000)).metadata.sumWindowsMs mustEqual 15000L
    }

    "Range === correctly constructed" >> {
      val range = Range(10000L, 60000L)
      range.metadata === (PatternMetadata.empty)
      range.valueType === (LongASTType)
    }
  }
}
