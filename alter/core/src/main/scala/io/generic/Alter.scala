package io.generic

import shapeless.{::, HList, HNil, LabelledGeneric}
import shapeless.tag.@@
import shapeless.labelled._

class Alter[T,InRepr<: HList,Repr<: HList,ReNames0<: HList](val gen: LabelledGeneric.Aux[T,InRepr]){
  type Out = Repr
  type ReNames = ReNames0

  def from(h: Repr)(implicit
    unNamer: Value.UnNamer.Aux[ReNames0,Repr,InRepr],
  ): T = gen.from(unNamer.apply(h))

  //important to notice that return type Tagged differs from Repr in in tagged types.
  def to[R1<:HList,UnTagged<: HList](t: T)(implicit
    reverse: Type.Reverse.Aux[ReNames0,R1],
    rename: Value.UnTag.NoReverse.UnNamer.Aux[R1,InRepr,UnTagged],
  ): UnTagged = rename.apply(gen.to(t))

  //important to notice that return type Tagged differs from Repr in in tagged types.
  def toFun[R1<:HList,UnTagged<: HList](implicit
    reverse: Type.Reverse.Aux[ReNames0,R1],
    rename: Value.UnTag.NoReverse.UnNamer.Aux[R1,InRepr,UnTagged],
  ): T => UnTagged = (t: T) => rename.apply(gen.to(t))

  def unname(implicit unNamer: Type.UnNamer[ReNames0,Repr]) = new Alter[T,InRepr,unNamer.Out,HNil](gen)

  def rename[Q<: HList](oldKey: String,newKey: String)(implicit
   rename: Type.Renamer.Aux[Repr,oldKey.type,newKey.type,Q]
  ): Alter[T,InRepr, Q, ::[@@[Symbol, oldKey.type], ::[@@[Symbol, newKey.type], ReNames0]]] =
    new Alter[T,InRepr,Q,::[Symbol @@ oldKey.type,::[Symbol @@ newKey.type, ReNames0]]](gen)

}


object Alter {

 final class Inline[T] {
    def gen[H <: HList](implicit gen: LabelledGeneric.Aux[T, H]): Alter[T, H, H, HNil] =
      new Alter[T, H, H, HNil](gen)
  }

  def apply[T] = new Inline[T]

}
