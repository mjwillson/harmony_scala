package com.yumptious.harmony

case class Interval(semitones: Int) extends Ordered[Interval] {
  def compare(other: Interval) = semitones - other.semitones

  def +(other: Pitch) = Pitch(semitones + other.midiNumber)
  def -(other: Pitch) = Pitch(semitones - other.midiNumber)

  def +(other: Interval) = Interval(semitones + other.semitones)
  def -(other: Interval) = Interval(semitones - other.semitones)

  def +(other: IntervalClass) = IntervalClass(semitones + other.semitones)
  def -(other: IntervalClass) = IntervalClass(semitones - other.semitones)


  def *(multiplier: Int) = Interval(semitones * multiplier)
  
  def unary_- = Interval(-semitones)
  def abs = Interval(Math.abs(semitones))
  
  def toIntervalClass = IntervalClass(semitones)
  def toInt = semitones
}
object Interval {
  val Unison = Interval(0)
  val Semitone = Interval(1)
  val MajorSecond = Interval(2)
  val MinorThird = Interval(3)
  val MajorThird = Interval(4)
  val PerfectFourth = Interval(5)
  val DiminishedFifth = Interval(6)
  val PerfectFifth = Interval(7)
  val AugmentedFifth = Interval(8)
  val MajorSixth = Interval(9)
  val MinorSeventh = Interval(10)
  val MajorSeventh = Interval(11)
  val Octave = Interval(12)

  implicit val ordering = Ordering.ordered[Interval]
}
