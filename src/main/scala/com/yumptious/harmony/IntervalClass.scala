package com.yumptious.harmony

// a note interval modulo 12
sealed abstract class IntervalClass(val semitones: Int) {
  def +(other: Int) = IntervalClass(semitones + other)
  def -(other: Int) = IntervalClass(semitones - other)

  def +(other: Interval) = IntervalClass(semitones + other.semitones)
  def -(other: Interval) = IntervalClass(semitones - other.semitones)

  def +(other: IntervalClass) = IntervalClass(semitones + other.semitones)
  def -(other: IntervalClass) = IntervalClass(semitones - other.semitones)

  def +(other: Pitch) = PitchClass(semitones + other.midiNumber)
  def +(other: PitchClass) = PitchClass(semitones + other.semitones)

  def *(multiplier : Int) = IntervalClass(semitones * multiplier)
  
  def unary_- = IntervalClass(-semitones)
  
  def toInterval(octaves : Int) = Interval(octaves*12 + semitones)
}
object IntervalClass {
  case object I0 extends IntervalClass(0) {}
  case object I1 extends IntervalClass(1) {}
  case object I2 extends IntervalClass(2) {}
  case object I3 extends IntervalClass(3) {}
  case object I4 extends IntervalClass(4) {}
  case object I5 extends IntervalClass(5) {}
  case object I6 extends IntervalClass(6) {}
  case object I7 extends IntervalClass(7) {}
  case object I8 extends IntervalClass(8) {}
  case object I9 extends IntervalClass(9) {}
  case object I10 extends IntervalClass(10) {}
  case object I11 extends IntervalClass(11) {}

  val instances = Array(I0,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11)
  def apply(semitones : Int) : IntervalClass = instances(mod(semitones, 12))
}
