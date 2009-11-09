package com.yumptious.harmony

/* conceptually, a set of pitches, modulo a transposition of the whole pitch_set by some number of octaves
  represented as: a set of intervals above the start of the octave of the lowest pitch.
  corresponds to something like, eg, Gmaj7 chord pattern (regardless of which octave's G it starts on)
  or, eg, C minor scale (regardless of which octave)]

  why do we not expose the set of intervals at all?
  because the choice of which PitchClass to use as the base of an octave (we use C) is an arbitrary one,
  so this is just an internal representation.
*/

class PitchSetModuloOctaves(pitches : Set[Pitch]) extends PartiallyOrdered[PitchSetModuloOctaves] {
  lazy private val intervals : Set[Interval] = {
    if (pitches.isEmpty) Set();
    else {
      val startOfOctaveOfMinPitch = new Pitch(pitches.min.octaveNumber * 12);
      pitches.map { pitch => pitch - startOfOctaveOfMinPitch }
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: PitchSetModuloOctaves =>
      (this eq that) ||
      (that canEqual this) &&
      (this.intervals == that.intervals)
    case _ =>
      false
  }
  def canEqual(that : Any) : Boolean = that.isInstanceOf[PitchSetModuloOctaves]
  override def hashCode() = intervals.hashCode

  def toPitchClassSet = pitches.map(_.toPitchClass)(PitchClassSet.canBuildFrom[Any])
  def toPitchSet(octaveNumber : Int) = {
    val startOfOctave = Pitch(octaveNumber*12)
    new PitchSet(intervals.map {interval => startOfOctave + interval})
  }
  def moduloTransposition = new PitchSetModuloTransposition(pitches)

  def isEmpty : Boolean = pitches.isEmpty
  def size : Int = pitches.size

  override def toString = toPitchSet(0).toString + ".moduloOctaves"
  
  // we can define a partial order like so:
  // are there any octave offsets which we can slide our interval set by so that it lines up with a subset of the other's interval set?
  // for example, we could say Dm is a subset of C13 chord shape (which requires a 1 octave shift to determine)
  def subsetOf(other : PitchSetModuloOctaves) : Boolean = {
    if (isEmpty) true;
    else if (other.isEmpty) false;
    else {
      val maxOctavesDiff = div(other.intervals.max.semitones, 12)
                            - div(intervals.max.semitones, 12)

      (0 to maxOctavesDiff).exists { octaveShift =>
        intervals.map {_ + Interval(octaveShift*12)}.subsetOf(other.intervals)
      }
    }
  }

  // boilerplate to expose subsetOf as a PartiallyOrdered
  def tryCompareTo[B >: PitchSetModuloOctaves <% PartiallyOrdered[B]](that : B) : Option[Int] = {
    that match {
      case that : PitchSetModuloOctaves => {
        if (this.intervals == that.intervals) Some(0);
        else if (this subsetOf that) Some(-1);
        else if (that subsetOf this) Some(1);
        else None;
      }
      case _ => None
    }
  }
  
}
object PitchSetModuloOctaves {
  def apply(args : Pitch*) : PitchSetModuloOctaves = new PitchSetModuloOctaves(args.toSet)

  implicit val partialOrdering : PartialOrdering[PitchSetModuloOctaves] = 
    new PartialOrdering[PitchSetModuloOctaves] {
      def tryCompare(x : PitchSetModuloOctaves, y : PitchSetModuloOctaves) = x tryCompareTo y
      def lteq(x : PitchSetModuloOctaves, y : PitchSetModuloOctaves) = x subsetOf y
    }

}