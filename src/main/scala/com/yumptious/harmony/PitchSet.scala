package com.yumptious.harmony

import scala.collection.immutable.{Set,HashSet}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.AddingBuilder
import scala.collection.SetLike

class PitchSet(val notes : Set[Pitch]) extends Set[Pitch] with SetLike[Pitch, PitchSet] {

  override def empty : PitchSet = new PitchSet(Set())
  def contains(elem : Pitch) : Boolean = notes.contains(elem)
  def iterator : Iterator[Pitch] = notes.iterator
  override def + (elem: Pitch) = new PitchSet(notes + elem)
  override def - (elem: Pitch) = new PitchSet(notes - elem)
  
  override def toString = mkString("PitchSet(", ", ", ")")
  
  def transpose(interval : Interval) : PitchSet = map(_ + interval)
  
  def toPitchClassSet = map(_.toPitchClass)(PitchClassSet.canBuildFrom[Any])
  def moduloOctaves       = new PitchSetModuloOctaves(notes)
  def moduloTransposition = new PitchSetModuloTransposition(notes)
}

object PitchSet extends AddableFactory[Pitch,PitchSet] {
  def empty = new PitchSet(Set())
  
  val equivalenceModuloTransposition = new Equiv[PitchSet] {
    def equiv(x : PitchSet, y : PitchSet) = x.moduloTransposition equals y.moduloTransposition
  }
  val equivalenceModuloOctaves = new Equiv[PitchSet] {
    def equiv(x : PitchSet, y : PitchSet) = x.moduloOctaves equals y.moduloOctaves
  }
}