package io.generic

import io.circe._
import shapeless._
import shapeless.tag.@@
import shapeless.tag
import shapeless.labelled._

package object circe extends shapes.LabelledHListInstances
    with generic.AutoDerivation
    with shapes.LabelledCoproductInstances
                          {

  implicit class CirceOps[T,Repr<:HList,H<: HList,R<: HList](private val alter: Alter[T,Repr,H,R]) extends AnyVal{

    def decoder[Q<: HList](implicit
      unNamer: Value.UnNamer.Aux[R,H,Repr],
      decoder: Decoder[H]//this must be the same as Decoder[alter.Out] but it is not. why aliases cause problems?
    ): Decoder[T] = decoder.map(out => alter.from(out)(unNamer))


    def encoder[R1<:HList,HUnT<: HList](implicit
      reverse: Type.Reverse.Aux[R,R1],
      rename: Value.UnTag.UnNamer.Aux[R1,Repr,HUnT],
      encoder: Encoder[HUnT],
    ): Encoder[T]  = Encoder.instance(t=> encoder(rename(alter.gen.to(t))))

  }

    implicit def unTagDecoderAndTagHList[T<: HList,T1<: HList,S<: String,H](implicit
      head: Decoder[T],
      tail: Decoder[T1],
      key: Witness.Aux[S]
    ): Decoder[FieldType[Symbol @@ S,T @@ H] :: T1] =
      Decoder.instance(c=>
        head.tryDecode(c.downField(key.value)).right.flatMap(t =>
          tail.tryDecode(c).right.map(t1 => field[Symbol @@ S](tag[H](t)) :: t1)))


}

