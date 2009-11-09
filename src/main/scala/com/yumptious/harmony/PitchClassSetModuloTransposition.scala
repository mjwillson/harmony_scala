package com.yumptious.harmony

class PitchClassSetModuloTransposition(val bits : Int) extends PartiallyOrdered[PitchClassSetModuloTransposition] {

  /* this is a 'binary 12-necklace' (a word of length 12 over a binary alphabet, taken modulo cyclic permutations).
     alternatively, a set of pitches-modulo-octave, treated (as a whole) modulo any transposition into another key
     alternatively, a shape of chord, without regard for which octave any given note of the chord is placed in.
     alternatively, a type of scale, as in "Major" or "Minor pentatonic", without regard for which note of the scale is seen as the starting note (that would be a 'mode') */

  private lazy val bitsRotations : Iterable[Int] = {
    (0 to 11).map { n => (bits>>n) | (bits<<(12-n)) & 4095 }
  }
  
  // a uniquely-determined (the least) representative of the equalivalence class of equivalent rotated PitchClassSet bitfields
  private lazy val bitsRepresentative : Int = bitsRotations.min

  def isEmpty : Boolean = bits == 0
  def size : Int = (0 to 11).count { n => (bits & (1<<n)) != 0 }
  def unary_- = new PitchClassSetModuloTransposition(~bits & 4095)
  def unary_~ = unary_-

  override def equals(that: Any): Boolean = that match {
    case that: PitchClassSetModuloTransposition =>
      (this eq that) ||
      (that canEqual this) &&
      (this.bitsRepresentative == that.bitsRepresentative)
    case _ =>
      false
  }
  def canEqual(that : Any) : Boolean = that.isInstanceOf[PitchClassSetModuloTransposition]
  override def hashCode() = bitsRepresentative

  def fixTransposition = new PitchClassSet(bitsRepresentative)

  // we can define a partial order like so:
  // x <= y if there is some cyclic permutation pi such that pi(representative(x)) <= representative(y)
  // for example, we could say the 'major' chord shape is a subset of the 'maj7' chord shape.
  def subsetOf(other : PitchClassSetModuloTransposition) : Boolean = {
    val othersBits = other.bits
    bitsRotations.exists { bitsRotation => ((~bitsRotation) | othersBits) == -1}
  }

  // why require such a f***ing complicated type signature for this one
  def tryCompareTo[B >: PitchClassSetModuloTransposition <% PartiallyOrdered[B]](that : B) : Option[Int] = {
    that match {
      case that : PitchClassSetModuloTransposition => {
        if (this.bitsRepresentative == that.bitsRepresentative) Some(0);
        else if (this subsetOf that) Some(-1);
        else if (that subsetOf this) Some(1);
        else None;
      }
      case _ => None
    }
  }

  override def toString = fixTransposition.toString + ".moduloTransposition"

  // there may be less than 12 in this set, if we have rotational symmetries
  def uniqueTranspositions : Set[PitchClassSet] = {
    bitsRotations.toSet.map { rotation : Int => new PitchClassSet(rotation) }
  }

  //def modes : Set[PitchClassSet] = fixPitchOctaves.modes
}
object PitchClassSetModuloTransposition {
  implicit val partialOrdering : PartialOrdering[PitchClassSetModuloTransposition] = 
    new PartialOrdering[PitchClassSetModuloTransposition] {
      def tryCompare(x : PitchClassSetModuloTransposition, y : PitchClassSetModuloTransposition) = x tryCompareTo y
      def lteq(x : PitchClassSetModuloTransposition, y : PitchClassSetModuloTransposition) = x subsetOf y
    }
}