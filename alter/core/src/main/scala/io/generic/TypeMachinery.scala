package io.generic

import shapeless.{HList,HNil, ::,LabelledGeneric}
import shapeless.labelled.FieldType
import shapeless.tag.@@


object Type{

  trait UnNamer[R<: HList,H<: HList]{type Out<: HList}

  object UnNamer{
    type Aux[Q<: HList, H<: HList,Out0<: HList] = UnNamer[Q,H]{ type Out = Out0 }

    implicit def nil[H1<: HList]: UnNamer.Aux[HNil,H1,H1] = new UnNamer[HNil,H1] { override type Out = H1 }

    implicit def cons[K1<: String,K2<: String,T<: HList,H<: HList,H1<: HList](implicit
       rename: Renamer.Aux[H,K2,K1,H1],
       rec: UnNamer[T,H1]
    ): UnNamer.Aux[@@[Symbol, K1] :: @@[Symbol,K2] :: T,H,rec.Out] =
      new UnNamer[@@[Symbol, K1] :: @@[Symbol,K2] :: T,H] { override type Out = rec.Out }
  }

  trait FlatRenamer[L<: HList, S1<: String, S2<: String]{type Out<: HList}
  object FlatRenamer {
    def apply[L <: HList, S1 <: String, S2 <: String](implicit replacer: FlatRenamer[L, S1, S2]): Aux[L, S1, S2, replacer.Out] = replacer

    type Aux[L <: HList, S1 <: String, S2 <: String, Out0] = FlatRenamer[L, S1, S2] {type Out = Out0}

        implicit def hlistReplacer1[T <: HList,S1<: String,S2<: String, U]: Aux[FieldType[Symbol @@ S1,U] :: T, S1, S2, FieldType[Symbol @@ S2,U] :: T] =
      new FlatRenamer[FieldType[Symbol @@ S1,U] :: T, S1, S2] { type Out = FieldType[Symbol @@ S2,U] :: T }

    implicit def hlistReplacer2[H, T <: HList,S1<: String,S2<: String,S3<: String, OutT <: HList](implicit
      ut : Aux[T, S1, S2,  OutT]
    ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,H] :: OutT] =
        new FlatRenamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] { type Out =  FieldType[Symbol @@ S3,H] :: OutT}

  }

  trait Renamer[L<: HList, S1<: String, S2<: String]{type Out<: HList}

  object Renamer {
    def apply[L<: HList, S1<: String, S2<: String](implicit replacer: Renamer[L, S1, S2]): Aux[L, S1, S2, replacer.Out] = replacer

    type Aux[L<: HList, S1<: String, S2<: String, Out0] = Renamer[L, S1, S2] { type Out = Out0 }

    implicit def hlistReplacer1[T <: HList,S1<: String,S2<: String, U]: Aux[FieldType[Symbol @@ S1,U] :: T, S1, S2, FieldType[Symbol @@ S2,U] :: T] =
      new Renamer[FieldType[Symbol @@ S1,U] :: T, S1, S2] { type Out = FieldType[Symbol @@ S2,U] :: T }

    implicit def hlistReplacer2[H, T <: HList,S1<: String,S2<: String,S3<: String, OutT <: HList](implicit
      ut : Aux[T, S1, S2,  OutT]
    ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,H] :: OutT] =
        new Renamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] { type Out =  FieldType[Symbol @@ S3,H] :: OutT}

    implicit def hListToCaseClassGen[S1<: String,S2<: String,S3<: String,H,HT<: HList, T <: HList, OutT <: HList](implicit
      ut : Aux[HT , S1, S2,  OutT],
      gen: LabelledGeneric.Aux[H,OutT],
    ): Aux[FieldType[Symbol @@ S3,HT @@ H] :: T, S1, S2,  FieldType[Symbol @@ S3, H] :: T] =
        new Renamer[FieldType[Symbol @@ S3,@@[HT,H]] :: T, S1, S2] {
          type Out =  FieldType[Symbol @@ S3, H] :: T
        }

    implicit def hlistReplacer3[H,HT<: HList, T <: HList,S1<: String,S2<: String,S3<: String, OutHT <: HList](implicit
      uh : Aux[HT, S1, S2,  OutHT]
    ): Aux[FieldType[Symbol @@ S3,HT @@ H] :: T, S1, S2, FieldType[Symbol @@ S3,OutHT  @@ H] :: T] =
        new Renamer[FieldType[Symbol @@ S3,HT @@ H] :: T, S1, S2] { type Out =  FieldType[Symbol @@ S3,OutHT  @@ H] :: T}

    implicit def caseClassToHListGen[S1<: String,S2<: String,S3<: String,H,HT<: HList, T <: HList, OutT <: HList](implicit
      gen: LabelledGeneric.Aux[H,HT],
      ut : FlatRenamer.Aux[HT, S1, S2, OutT]
    ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,OutT @@ H] :: T] =
        new Renamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] {
          type Out = FieldType[Symbol @@ S3,OutT @@ H] :: T
        }

  }

  trait Reverse[L <: HList]{ type Out <: HList }

  object Reverse {
    def apply[L <: HList](implicit reverse: Reverse[L]): Aux[L, reverse.Out] = reverse

    type Aux[L <: HList, Out0 <: HList] = Reverse[L] { type Out = Out0 }

    implicit def reverse[L <: HList, Out0 <: HList](implicit reverse : Reverse0[HNil, L, Out0]): Aux[L, Out0] =
      new Reverse[L] { type Out = Out0 }

    trait Reverse0[Acc <: HList, L <: HList, Out <: HList]

    object Reverse0 {
      implicit def hnilReverse[Out <: HList]: Reverse0[Out, HNil, Out] = new Reverse0[Out, HNil, Out] {}

      implicit def hlistReverse[Acc <: HList, InH, InT <: HList, Out <: HList](implicit
        rt : Reverse0[InH :: Acc, InT, Out]
      ): Reverse0[Acc, InH :: InT, Out] = new Reverse0[Acc, InH :: InT, Out] {}
    }
  }

}
