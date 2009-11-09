package com.yumptious.harmony

/* conceptually, a set of pitches, modulo a transposition of the whole pitch_set by some number of semitones (into a different key, say)
   represented as: a set of intervals above the bottom pitch 0
   corresponds to a 'shape of chord' - eg maj7, sus2 etc
   or, a 'mode' of a scale, eg 'the Dorian mode of the major scale' - a scale with a particular note of the scale fixed as the starting note

   why do we not directly extend Set[Interval] or Iterable[Interval] for the set of intervals?
   because it's not clear what +, - or newBuilder should do, since any newly-constructed set of intervals
   would need renormalizing against its lowest member, breaking the usual contract implied for these
   methods, eg (x + a).contains(a) == true.
   so we just expose a member Set[Interval] which you can use.
   */
import scala.collection.Seq
class PitchSetModuloTransposition(pitches : Set[Pitch]) extends PartiallyOrdered[PitchSetModuloTransposition] {

  lazy val intervals : Set[Interval] = {
    if (pitches.isEmpty) Set();
    else {
      val minPitch = pitches.min;
      pitches.map { pitch => pitch - minPitch }
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: PitchSetModuloTransposition =>
      (this eq that) ||
      (that canEqual this) &&
      (this.intervals == that.intervals)
    case _ =>
      false
  }
  def canEqual(that : Any) : Boolean = that.isInstanceOf[PitchSetModuloTransposition]
  override def hashCode() = intervals.hashCode

  def toPitchClassSetModuloTransposition =
    pitches.map(_.toPitchClass)(PitchClassSet.canBuildFrom[Any]).moduloTransposition
  def toPitchClassSet(basePitchClass : PitchClass) = 
    intervals.map(basePitchClass + _)(PitchClassSet.canBuildFrom[Any])
  def toPitchSet(basePitch : Pitch) : PitchSet =
    intervals.map(basePitch + _)(PitchSet.canBuildFrom[Any])

  def isEmpty : Boolean = pitches.isEmpty
  def size : Int = pitches.size

  override def toString = toPitchSet(Pitch(0)).toString + ".moduloTransposition"
  
  // we can define a partial order like so:
  // are there any offsets which we can slide our interval set by so that it lines up with a subset of the other's interval set?
  // for example, we could say Em is a subset of Cmaj7 chord shape (which requires a 4 semitone shift to the base note to determine)
  def subsetOf(other : PitchSetModuloTransposition) : Boolean = {
    if (isEmpty) true
    else if (other.isEmpty) false
    else {
      val maxDiff : Interval = other.intervals.max - intervals.max

      (0 to maxDiff.semitones).exists { offset =>
        intervals.map {_ + Interval(offset)}.subsetOf(other.intervals)
      }
    }
  }

  // boilerplate to expose subsetOf as a PartiallyOrdered
  def tryCompareTo[B >: PitchSetModuloTransposition <% PartiallyOrdered[B]](that : B) : Option[Int] = {
    that match {
      case that : PitchSetModuloTransposition => {
        if (this.intervals == that.intervals) Some(0);
        else if (this subsetOf that) Some(-1);
        else if (that subsetOf this) Some(1);
        else None;
      }
      case _ => None
    }
  }
  
  // little convenience. note that this, too, uniquely identifies the equivalence class.
  def intervalsBetweenPitches : Option[Seq[Interval]] = {
    if (isEmpty) None
    else {
      val sortedPitches = pitches.toList.sortWith {_<_}
      Some(sortedPitches.zip(sortedPitches.tail).map {case (x,y) => y-x})
    }
  }
}
object PitchSetModuloTransposition {
  def apply(args : Pitch*) : PitchSetModuloTransposition = new PitchSetModuloTransposition(args.toSet)

  implicit val partialOrdering : PartialOrdering[PitchSetModuloTransposition] = 
    new PartialOrdering[PitchSetModuloTransposition] {
      def tryCompare(x : PitchSetModuloTransposition, y : PitchSetModuloTransposition) = x tryCompareTo y
      def lteq(x : PitchSetModuloTransposition, y : PitchSetModuloTransposition) = x subsetOf y
    }

}