package dsl

sealed trait AggregateFn extends Product with Serializable
case object Sum          extends AggregateFn
case object Count        extends AggregateFn
case object Avg          extends AggregateFn
case object Lag          extends AggregateFn

object AggregateFn {

  def fromSymbol(name: Symbol): Either[String, AggregateFn] = name match {
    case 'sum   => Right(Sum)
    case 'count => Right(Count)
    case 'avg   => Right(Avg)
    case 'lag   => Right(Lag)
    case _      => Left(s"Unknown aggregator '$name'")
  }
}
