/* package dsl

import org.specs2._

class FunctionRegistryTest extends mutable.Specification {
  val funReg = DefaultFunctionRegistry
  //implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-6)

  "Function registry specification " >> {
    "Function registry boolean functions === callable" >> {
      funReg.functions(('and, Seq(BooleanASTType, BooleanASTType)))._1(Seq(true, false)) === Some(false)
      funReg.functions(('or, Seq(BooleanASTType, BooleanASTType)))._1(Seq(false, true)) === Some(true)
      funReg.functions(('xor, Seq(BooleanASTType, BooleanASTType)))._1(Seq(true, true)) === Some(false)
      funReg.functions(('eq, Seq(BooleanASTType, BooleanASTType)))._1(Seq(true, true)) === Some(true)
      funReg.functions(('neq, Seq(BooleanASTType, BooleanASTType)))._1(Seq(true, true)) === Some(false)
      funReg.functions(('not, Seq(BooleanASTType)))._1(Seq(true)) === Some(false)
    }

    "Function registry arithmetic functions with double === callable" >> {
      funReg.functions(('add, Seq(DoubleASTType, DoubleASTType)))._1(Seq(5.0, 8.0)) === Some(13.0)
      funReg.functions(('sub, Seq(DoubleASTType, DoubleASTType)))._1(Seq(29.0, 16.0)) === Some(13.0)
      funReg.functions(('mul, Seq(DoubleASTType, DoubleASTType)))._1(Seq(13.0, 1.0)) === Some(13.0)
      funReg.functions(('div, Seq(DoubleASTType, DoubleASTType)))._1(Seq(78.0, 6.0)) === Some(13.0)
    }

    "Function registry arithmetic functions with long === callable" >> {
      funReg.functions(('add, Seq(LongASTType, LongASTType)))._1(Seq(5L, 8L)) === Some(13L)
      funReg.functions(('sub, Seq(LongASTType, LongASTType)))._1(Seq(29L, 16L)) === Some(13L)
      funReg.functions(('mul, Seq(LongASTType, LongASTType)))._1(Seq(13L, 1L)) === Some(13L)
      funReg.functions(('div, Seq(LongASTType, LongASTType)))._1(Seq(78L, 6L)) === Some(13L)
    }

    "Function registry arithmetic functions with int === callable" >> {
      funReg.functions(('add, Seq(IntASTType, IntASTType)))._1(Seq(5, 8)) === Some(13)
      funReg.functions(('sub, Seq(IntASTType, IntASTType)))._1(Seq(29, 16)) === Some(13)
      funReg.functions(('mul, Seq(IntASTType, IntASTType)))._1(Seq(13, 1)) === Some(13)
      funReg.functions(('div, Seq(IntASTType, IntASTType)))._1(Seq(78, 6)) === Some(13)
    }

    "Function registry arithmetic functions with mixed types === callable" >> {
      funReg.functions(('add, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](5.0, 8)) === Some(13.0)
      funReg.functions(('sub, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](29.0, 16)) === Some(13.0)
      funReg.functions(('mul, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](13.0, 1)) === Some(13.0)
      funReg.functions(('div, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](78.0, 6)) === Some(13.0)
    }

    "Math functions === callable" >> {
      // We need `.asInstanceOf[Double]` here to enable tolerant comparison
      funReg
        .functions(('abs, Seq(DoubleASTType)))
        ._1(Seq(-1.0))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (1.0 +/- 1e-5)
      funReg
        .functions(('sin, Seq(DoubleASTType)))
        ._1(Seq(Math.PI / 2))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (1.0 +/- 1e-5)
      funReg
        .functions(('cos, Seq(DoubleASTType)))
        ._1(Seq(Math.PI / 2))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (0.0 +/- 1e-5)
      funReg
        .functions(('tan, Seq(DoubleASTType)))
        ._1(Seq(Math.PI / 4))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (1.0 +/- 1e-5)
      funReg
        .functions(('tg, Seq(DoubleASTType)))
        ._1(Seq(Math.PI / 4))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (1.0 +/- 1e-5)
      funReg
        .functions(('cot, Seq(DoubleASTType)))
        ._1(Seq(Math.PI / 4))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (1.0 +/- 1e-5)
      funReg
        .functions(('ctg, Seq(DoubleASTType)))
        ._1(Seq(Math.PI / 4))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (1.0 +/- 1e-5)
      funReg
        .functions(('sind, Seq(DoubleASTType)))
        ._1(Seq(30.0))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (0.5 +/- 1e-5)
      funReg
        .functions(('cosd, Seq(DoubleASTType)))
        ._1(Seq(60.0))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (0.5 +/- 1e-5)
      funReg
        .functions(('tand, Seq(DoubleASTType)))
        ._1(Seq(45.0))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (1.0 +/- 1e-5)
      funReg
        .functions(('tgd, Seq(DoubleASTType)))
        ._1(Seq(0.0))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (0.0 +/- 1e-5)
      funReg
        .functions(('cotd, Seq(DoubleASTType)))
        ._1(Seq(45.0))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (1.0 +/- 1e-5)
      funReg
        .functions(('ctgd, Seq(DoubleASTType)))
        ._1(Seq(90.0))
        .getOrElse(Double.NaN)
        .asInstanceOf[Double] must be ~ (0.0 +/- 1e-5)
    }

    "Function registry comparison functions with double === callable" >> {
      funReg.functions(('lt, Seq(DoubleASTType, DoubleASTType)))._1(Seq(5.0, 8.0)) === Some(true)
      funReg.functions(('le, Seq(DoubleASTType, DoubleASTType)))._1(Seq(29.0, 18.0)) === Some(false)
      funReg.functions(('gt, Seq(DoubleASTType, DoubleASTType)))._1(Seq(13.0, 12.0)) === Some(true)
      funReg.functions(('ge, Seq(DoubleASTType, DoubleASTType)))._1(Seq(5.0, 6.0)) === Some(false)
      funReg.functions(('eq, Seq(DoubleASTType, DoubleASTType)))._1(Seq(4.0, 4.0)) === Some(true)
      funReg.functions(('ne, Seq(DoubleASTType, DoubleASTType)))._1(Seq(21.0, 6.0)) === Some(true)
    }

    "Function registry comparison functions with mixed types === callable" >> {
      funReg.functions(('lt, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](5.0, 8)) === Some(true)
      funReg.functions(('le, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](29.0, 18)) === Some(false)
      funReg.functions(('gt, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](13.0, 12)) === Some(true)
      funReg.functions(('ge, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](5.0, 6)) === Some(false)
      funReg.functions(('eq, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](4.0, 4)) === Some(true)
      funReg.functions(('ne, Seq(DoubleASTType, IntASTType)))._1(Seq[Any](21.0, 6)) === Some(true)
    }

    "Function registry concatenation must work" >> {
      // currently no other registries exist
      funReg ++ funReg mustEqual funReg
    }

    "Function registry reducers === callable" >> {
      funReg.reducers(('sumof, DoubleASTType))._1(Some(5.0), Some(8.0)) === Some(13.0)
      funReg.reducers(('minof, DoubleASTType))._1(Some(5.0), Some(8.0)) === Some(5.0)
      funReg.reducers(('maxof, DoubleASTType))._1(Some(5.0), Some(8.0)) === Some(8.0)
      funReg.reducers(('sumof, DoubleASTType))._1(None, Some(8.0)) === None
      funReg.reducers(('minof, DoubleASTType))._1(None, Some(8.0)) === None
      funReg.reducers(('maxof, DoubleASTType))._1(None, Some(8.0)) === None
    }
  }
}
 */
