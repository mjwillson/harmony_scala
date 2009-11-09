package com.yumptious.harmony

import scala.collection.immutable.{Set,HashSet}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.SetLike

class PitchClassSet(val bits : Int) extends Set[PitchClass] with SetLike[PitchClass, PitchClassSet] {

  override def empty : PitchClassSet = new PitchClassSet(0)

  def contains(elem : PitchClass) : Boolean = (bits & (1 << elem.semitones)) != 0

  def iterator : Iterator[PitchClass] = {
    val ns = for (n <- 0 to 11 if (bits & (1 << n)) != 0) yield PitchClass(n);
    ns.iterator
  }

  override def + (elem: PitchClass) = new PitchClassSet(bits | (1 << elem.semitones))
  override def - (elem: PitchClass) = new PitchClassSet(bits & ~(1 << elem.semitones))
  
  def unary_- = new PitchClassSet(~bits & 4095)
  def unary_~ = unary_-

  override def equals(that: Any): Boolean = that match {
    case that: PitchClassSet =>
      (this eq that) ||
      (that canEqual this) &&
      (this.bits == that.bits)
    case _ =>
      false
  }
  
  override def hashCode() = bits

  override def isEmpty : Boolean = bits == 0

  def subsetOf(that : PitchClassSet) : Boolean = (~bits | that.bits) == -1
  def diff(that : PitchClassSet)      = new PitchClassSet(that.bits & ~this.bits)
  def union(that : PitchClassSet)     = new PitchClassSet(bits & that.bits)
  def intersect(that : PitchClassSet) = new PitchClassSet(bits | that.bits)

  override def toString = mkString("PitchClassSet(", ", ", ")")
  
  def transpose(intervalClass : IntervalClass) : PitchClassSet = {
    val semitones = intervalClass.semitones;
    new PitchClassSet((bits << semitones) & 4095 | (bits >> (12-semitones)))
  }
  
  def transpose(interval : Interval) : PitchClassSet = transpose(interval.toIntervalClass)

  def moduloTransposition = new PitchClassSetModuloTransposition(bits)
}

object PitchClassSet extends AddableFactory[PitchClass,PitchClassSet] {
  def empty = new PitchClassSet(0)
  
  val equivalenceModuloTransposition = new Equiv[PitchClassSet] {
    def equiv(x : PitchClassSet, y : PitchClassSet) = x.moduloTransposition equals y.moduloTransposition
  }
  
  implicit val partialOrdering : PartialOrdering[PitchClassSetModuloTransposition] = 
    new PartialOrdering[PitchClassSetModuloTransposition] {
      def tryCompare(x : PitchClassSetModuloTransposition, y : PitchClassSetModuloTransposition) = x tryCompareTo y
      def lteq(x : PitchClassSetModuloTransposition, y : PitchClassSetModuloTransposition) = x subsetOf y
    }
}