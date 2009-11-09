package com.yumptious.harmony

case class Pitch(midiNumber: Int) extends Ordered[Pitch] {
  def compare(other: Pitch) = midiNumber - other.midiNumber

  def +(other: Int) = Pitch(midiNumber + other)
  def -(other: Int) = Pitch(midiNumber - other)

  def +(other: Interval) = Pitch(midiNumber + other.semitones)
  def -(other: Interval) = Pitch(midiNumber - other.semitones)

  def +(other: IntervalClass) = PitchClass(midiNumber + other.semitones)
  def -(other: IntervalClass) = PitchClass(midiNumber - other.semitones)

  def -(other: Pitch)      = Interval(midiNumber - other.midiNumber)
  def -(other: PitchClass) = IntervalClass(midiNumber - other.semitones)

  def octaveNumber = div(midiNumber, 12)

  def toPitchClass = PitchClass(midiNumber)
  
  override def toString = toPitchClass.toString + octaveNumber.toString
}

object Pitch {
  implicit val ordering : Ordering[Pitch] = Ordering.ordered[Pitch]
}
