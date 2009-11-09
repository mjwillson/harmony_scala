package com.yumptious.harmony

// a note modulo 12
sealed abstract class PitchClass(val semitones : Int) {
  def +(other: Int) = PitchClass(semitones + other)
  def -(other: Int) = PitchClass(semitones - other)

  def +(other: Interval) = PitchClass(semitones + other.semitones)
  def -(other: Interval) = PitchClass(semitones - other.semitones)

  def +(other: IntervalClass) = PitchClass(semitones + other.semitones)
  def -(other: IntervalClass) = PitchClass(semitones - other.semitones)

  def -(other: PitchClass) = IntervalClass(semitones - other.semitones)
  def -(other: Pitch)      = IntervalClass(semitones - other.midiNumber)
  
  def toPitch(octaveNumber : Int) = Pitch(octaveNumber*12 + semitones)
}
object PitchClass {
  case object C       extends PitchClass(0) {}
  case object C_sharp extends PitchClass(1) {}
  case object D       extends PitchClass(2) {}
  case object D_sharp extends PitchClass(3) {}
  case object E       extends PitchClass(4) {}
  case object F       extends PitchClass(5) {}
  case object F_sharp extends PitchClass(6) {}
  case object G       extends PitchClass(7) {}
  case object G_sharp extends PitchClass(8) {}
  case object A       extends PitchClass(9) {}
  case object A_sharp extends PitchClass(10) {}
  case object B       extends PitchClass(11) {}
  val D_flat = C_sharp
  val E_flat = D_sharp
  val G_flat = F_sharp
  val A_flat = G_sharp
  val B_flat = A_sharp

  val instances : Array[PitchClass] = Array(C, C_sharp, D, D_sharp, E, F, F_sharp, G, G_sharp, A, A_sharp, B)

  def apply(semitones: Int) = instances(mod(semitones, 12))
}