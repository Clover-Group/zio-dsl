package dsl

case class PatternMetadata(fields: Set[Symbol], sumWindowsMs: Long) {
  def +(other: PatternMetadata): PatternMetadata =
    PatternMetadata(fields ++ other.fields, sumWindowsMs + other.sumWindowsMs)
}

object PatternMetadata {
  val empty = PatternMetadata(Set.empty, 0L)
}
