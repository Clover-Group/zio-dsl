package dsl

// TimeIntervals, windows, etc.
// TODO: Remove this stuff, it's temporary. It must be in another project

case class Window(toMillis: Long) extends Serializable

object MinWindow extends Window(toMillis = 0L)

object MaxWindow extends Window(toMillis = Long.MaxValue)

object Window {
  def less(w: Window) = TimeInterval(max = w)

  def more(w: Window) = TimeInterval(min = w)
}

trait Interval[T] {
  def contains(value: T): Boolean
  def isInfinite: Boolean
}

/** Inclusive-exclusive interval of time */
case class TimeInterval(min: Long, max: Long) extends Interval[Long] {
  override def contains(w: Long): Boolean = w >= min && w <= max

  override def isInfinite: Boolean = max == MaxWindow.toMillis
}

object TimeInterval {
  def apply(min: Window = MinWindow, max: Window = MaxWindow): TimeInterval = TimeInterval(min.toMillis, max.toMillis)

  val MaxInterval = TimeInterval(MaxWindow, MaxWindow)

  def less(value: Window): TimeInterval = TimeInterval(0L, value.toMillis)

  def more(value: Window): TimeInterval = TimeInterval(value.toMillis, Long.MaxValue)
}

case class NumericInterval[T](min: T, max: T)(implicit num: Numeric[T]) extends Interval[T] {
  override def contains(value: T): Boolean = num.gteq(value, min) && num.lteq(value, max)

  override def isInfinite: Boolean = num.toLong(max) == Long.MaxValue
}

object NumericInterval {
  def less[T](value: T)(implicit num: Numeric[T]): NumericInterval[T] = NumericInterval(num.zero, value)

  def more[T](value: T)(implicit num: Numeric[T]): NumericInterval[T] = NumericInterval(value, ???)
}
