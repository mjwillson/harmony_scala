package com.yumptious.harmony

import scala.collection.mutable.AddingBuilder
import scala.collection.generic.{Addable,CanBuildFrom}
import scala.collection.{Iterable,IterableLike}

abstract class AddableFactory[A, CC <: Addable[A,CC] with Iterable[A] with IterableLike[A,CC]] {
  def newBuilder: AddingBuilder[A, CC] = new AddingBuilder[A,CC](empty)

  def empty: CC
 
  def apply(args: A*): CC = {
    val b = newBuilder
    b ++= args
    b.result
  }     

  def canBuildFrom[T] : CanBuildFrom[T,A,CC] = {
    new CanBuildFrom[T,A,CC] {
      def apply()        = newBuilder
      def apply(from: T) = newBuilder
    }
  }
  
  implicit def canBuildFromSelf = canBuildFrom[CC]
}