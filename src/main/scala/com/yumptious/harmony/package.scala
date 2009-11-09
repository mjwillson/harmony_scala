//import com.yumptious.harmony.{Interval,Pitch}
package com.yumptious;

package object harmony {
  implicit def intToInterval(semitones: Int) : Interval = Interval(semitones)
  implicit def noteToPitchClass(note: Pitch) = note.toPitchClass
  implicit def noteIntervalToIntervalClass(noteInterval : Interval) = noteInterval.toIntervalClass
  
  // scala's % and / do the wrong damn thing with negative numbers.
  
  def mod(a : Int, b : Int) = {
    val badModulus = a % b
    if (badModulus < 0) badModulus + b else badModulus
  }
  
  def div(a : Int, b : Int) = {
    if (a < 0) (a-b+1)/b else a/b
  }
}