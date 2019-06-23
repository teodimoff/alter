package io.generic

import shapeless.{HList,HNil, ::,LabelledGeneric}
import shapeless.labelled.{FieldType, field}
import shapeless.tag.@@
import shapeless.Lazy
import shapeless.tag

object Value{

  trait UnNamer[R<: HList,H<: HList]{
    type Out<: HList
    def apply(h: H): Out
  }

  object UnNamer{
    type Aux[Q<: HList, H<: HList,Out0<: HList] = UnNamer[Q,H]{ type Out = Out0 }

    implicit def nil[H1<: HList]: UnNamer.Aux[HNil,H1,H1] = new UnNamer[HNil,H1] {
      type Out = H1
      def apply(h: H1): H1 = h
    }

    implicit def cons[K1<: String,K2<: String,T<: HList,H<: HList,H1<: HList](implicit
       rename: Renamer.Aux[H,K2,K1,H1],
       rec: UnNamer[T,H1]
    ): UnNamer.Aux[@@[Symbol, K1] :: @@[Symbol,K2] :: T,H,rec.Out] =
      new UnNamer[@@[Symbol, K1] :: @@[Symbol,K2] :: T,H] {
        override type Out = rec.Out
        def apply(h: H): rec.Out = rec.apply(rename.apply(h))
      }
  }

  trait FlatRenamer[L<: HList, S1<: String, S2<: String]{
    type Out<: HList
    def apply(l: L): Out
  }

  object FlatRenamer {
    def apply[L <: HList, S1 <: String, S2 <: String](implicit replacer: FlatRenamer[L, S1, S2]): Aux[L, S1, S2, replacer.Out] = replacer

    type Aux[L <: HList, S1 <: String, S2 <: String, Out0] = FlatRenamer[L, S1, S2] {type Out = Out0}

        implicit def hlistReplacer1[T <: HList,S1<: String,S2<: String, U]: Aux[FieldType[Symbol @@ S1,U] :: T, S1, S2, FieldType[Symbol @@ S2,U] :: T] =
      new FlatRenamer[FieldType[Symbol @@ S1,U] :: T, S1, S2] {
        type Out = FieldType[Symbol @@ S2,U] :: T
        def apply(l: FieldType[Symbol @@ S1,U] :: T): FieldType[Symbol @@ S2,U] :: T =
          field[Symbol @@ S2](l.head: U ) :: l.tail
      }

    implicit def hlistReplacer2[H, T <: HList,S1<: String,S2<: String,S3<: String, OutT <: HList](implicit
      ut : Aux[T, S1, S2,  OutT]
    ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,H] :: OutT] =
        new FlatRenamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] {
          type Out =  FieldType[Symbol @@ S3,H] :: OutT
          def apply(l: FieldType[Symbol @@ S3,H] :: T): FieldType[Symbol @@ S3,H] :: OutT =
            l.head :: ut.apply(l.tail)
        }

  }

  trait Renamer[L<: HList, S1<: String, S2<: String]{
    type Out<: HList
    def apply(l: L): Out
  }

  object Renamer extends LowerLevel{
    def apply[L<: HList, S1<: String, S2<: String](implicit replacer: Renamer[L, S1, S2]): Aux[L, S1, S2, replacer.Out] = replacer

    type Aux[L<: HList, S1<: String, S2<: String, Out0] = Renamer[L, S1, S2] { type Out = Out0 }

    implicit def hlistReplacer1[T <: HList,S1<: String,S2<: String, U]: Aux[FieldType[Symbol @@ S1,U] :: T, S1, S2, FieldType[Symbol @@ S2,U] :: T] =
      new Renamer[FieldType[Symbol @@ S1,U] :: T, S1, S2] {
        type Out = FieldType[Symbol @@ S2,U] :: T
        def apply(l: FieldType[Symbol @@ S1,U] :: T): Out =
          field[Symbol @@ S2](l.head: U ) :: l.tail
      }

    implicit def hlistReplacer2[H, T <: HList,S1<: String,S2<: String,S3<: String, OutT <: HList](implicit
      ut : Aux[T, S1, S2,  OutT]
    ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,H] :: OutT] =
        new Renamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] {
          type Out =  FieldType[Symbol @@ S3,H] :: OutT
          def apply(l: FieldType[Symbol @@ S3,H] :: T): FieldType[Symbol @@ S3,H] :: OutT =
            l.head :: ut.apply(l.tail)
        }

    implicit def hListToCaseClassGen[S1<: String,S2<: String,S3<: String,H,HT<: HList, T <: HList, OutT <: HList](implicit
      ut : Aux[HT , S1, S2,  OutT],
      gen: LabelledGeneric.Aux[H,OutT],
    ): Aux[FieldType[Symbol @@ S3,HT @@ H] :: T, S1, S2,  FieldType[Symbol @@ S3, H] :: T] =
        new Renamer[FieldType[Symbol @@ S3,@@[HT,H]] :: T, S1, S2] {
          type Out =  FieldType[Symbol @@ S3, H] :: T
          def apply(l: FieldType[Symbol @@ S3,@@[HT, H]] :: T): FieldType[Symbol @@ S3, H] :: T =
            field[Symbol @@ S3](gen.from(ut.apply(l.head))) :: l.tail
        }


    implicit def caseClassToHListGen[S1<: String,S2<: String,S3<: String,H,HT<: HList, T <: HList, OutT <: HList](implicit
      gen: LabelledGeneric.Aux[H,HT],
      ut : FlatRenamer.Aux[HT, S1, S2, OutT]
    ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,OutT @@ H] :: T] =
        new Renamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] {
          type Out = FieldType[Symbol @@ S3,@@[OutT ,H]] :: T
          def apply(l: FieldType[Symbol @@ S3,H] :: T): FieldType[Symbol @@ S3,OutT @@ H] :: T =
            field[Symbol @@ S3](tag[H](ut.apply(gen.to(l.head)))) :: l.tail
        }

  }

  trait LowerLevel{
    implicit def hlistReplacer3[H,HT<: HList, T <: HList,S1<: String,S2<: String,S3<: String, OutHT <: HList](implicit
      uh : Renamer.Aux[HT, S1, S2,  OutHT]
    ): Renamer.Aux[FieldType[Symbol @@ S3,HT @@ H] :: T, S1, S2, FieldType[Symbol @@ S3,OutHT  @@ H] :: T] =
        new Renamer[FieldType[Symbol @@ S3,HT @@ H] :: T, S1, S2] {
          type Out =  FieldType[Symbol @@ S3,OutHT  @@ H] :: T
          def apply(l: FieldType[Symbol @@ S3,HT @@ H] :: T): FieldType[Symbol @@ S3,OutHT  @@ H] :: T =
            field[Symbol @@ S3](tag[H](uh.apply(l.head))) :: l.tail
        }
    }


  object UnTag{
    import shapeless.ops.hlist.Reverse

    trait UnNamer[R<: HList,H<: HList]{
      type Out<: HList
      def apply(h: H): Out
    }

    object UnNamer{
      type Aux[Q<: HList, H<: HList,Out0<: HList] = UnNamer[Q,H]{ type Out = Out0 }

      implicit def nil[H1<: HList,H1R<: HList](implicit
        rev: Reverse.Aux[H1,H1R]
      ): UnNamer.Aux[HNil,H1,H1R] = new UnNamer[HNil,H1] {
         type Out = H1R
         def apply(h: H1): H1R = rev.apply(h)
      }

      implicit def cons[K1<: String,K2<: String,T<: HList,H<: HList,H1<: HList](implicit
         rename: Renamer.Aux[H,K2,K1,H1],
         rec: Lazy[UnNamer[T,H1]]
      ): UnNamer.Aux[@@[Symbol, K1] :: @@[Symbol,K2] :: T,H,rec.value.Out] =
        new UnNamer[@@[Symbol, K1] :: @@[Symbol,K2] :: T,H] {
         override type Out = rec.value.Out
           def apply(h: H): rec.value.Out = rec.value.apply(rename.apply(h))
        }
    }

    trait Renamer[L<: HList, S1<: String, S2<: String]{
        type Out<: HList
        def apply(l: L): Out
      }

    object Renamer extends LowerLevel{

      def apply[L<: HList, S1<: String, S2<: String](implicit replacer: Renamer[L, S1, S2]): Aux[L, S1, S2, replacer.Out] = replacer

      type Aux[L<: HList, S1<: String, S2<: String, Out0<: HList] = Renamer[L, S1, S2] { type Out = Out0 }

      implicit def hlistReplacer1[T <: HList,S1<: String,S2<: String, U]: Aux[FieldType[Symbol @@ S1,U] :: T, S1, S2, FieldType[Symbol @@ S2,U] :: T] =
        new Renamer[FieldType[Symbol @@ S1,U] :: T, S1, S2] {
          type Out = FieldType[Symbol @@ S2,U] :: T
           def apply(l: FieldType[Symbol @@ S1,U] :: T): FieldType[Symbol @@ S2,U] :: T =
             field[Symbol @@ S2](l.head: U ) :: l.tail
        }

      implicit def hlistReplacer2[H, T <: HList,S1<: String,S2<: String,S3<: String, OutT <: HList](implicit
        ut : Aux[T, S1, S2,  OutT],
      ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,H] :: OutT] =
          new Renamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] {
            type Out =  FieldType[Symbol @@ S3,H] :: OutT
             def apply(l: FieldType[Symbol @@ S3,H] :: T): FieldType[Symbol @@ S3,H] :: OutT =
               l.head :: ut.apply(l.tail)
          }


      implicit def caseClassToHListGen[S1<: String,S2<: String,S3<: String,H,HT<: HList, T <: HList, OutT <: HList, OutTR <: HList](implicit
        gen: LabelledGeneric.Aux[H,HT],
        ut : FlatRenamer.Aux[HT, S1, S2,  OutT],rev: Reverse.Aux[OutT,OutTR]
      ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,OutTR] :: T] =
          new Renamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] {
            type Out = FieldType[Symbol @@ S3,OutTR] :: T
            def apply(l: FieldType[Symbol @@ S3,H] :: T): FieldType[Symbol @@ S3,OutTR] :: T =
              field[Symbol @@ S3](rev.apply(ut.apply(gen.to(l.head)))) :: l.tail
          }
    }

    trait LowerLevel{
    implicit def hlistReplacer3[H,HT<: HList, T <: HList,S1<: String,S2<: String,S3<: String, OutHT <: HList](implicit
      uh : Renamer.Aux[HT, S1, S2,  OutHT]
    ): Renamer.Aux[FieldType[Symbol @@ S3,HT] :: T, S1, S2, FieldType[Symbol @@ S3,OutHT] :: T] =
        new Renamer[FieldType[Symbol @@ S3,HT] :: T, S1, S2] {
          type Out =  FieldType[Symbol @@ S3,OutHT] :: T
          def apply(l: FieldType[Symbol @@ S3,HT] :: T): FieldType[Symbol @@ S3,OutHT] :: T =
            field[Symbol @@ S3](tag[H](uh.apply(l.head))) :: l.tail
        }
    }

        object NoReverse{

      trait UnNamer[R<: HList,H<: HList]{
        type Out<: HList
        def apply(h: H): Out
      }

      object UnNamer{
        type Aux[Q<: HList, H<: HList,Out0<: HList] = UnNamer[Q,H]{ type Out = Out0 }

        implicit def nil[H1<: HList]: UnNamer.Aux[HNil,H1,H1] = new UnNamer[HNil,H1] {
           type Out = H1
           def apply(h: H1): H1 = h
        }

        implicit def cons[K1<: String,K2<: String,T<: HList,H<: HList,H1<: HList](implicit
           rename: Renamer.Aux[H,K2,K1,H1],
           rec: Lazy[UnNamer[T,H1]]
        ): UnNamer.Aux[@@[Symbol, K1] :: @@[Symbol,K2] :: T,H,rec.value.Out] =
          new UnNamer[@@[Symbol, K1] :: @@[Symbol,K2] :: T,H] {
           override type Out = rec.value.Out
             def apply(h: H): rec.value.Out = rec.value.apply(rename.apply(h))
          }
      }

      trait Renamer[L<: HList, S1<: String, S2<: String]{
          type Out<: HList
          def apply(l: L): Out
        }

      object Renamer extends LowerLevel{

        def apply[L<: HList, S1<: String, S2<: String](implicit replacer: Renamer[L, S1, S2]): Aux[L, S1, S2, replacer.Out] = replacer

        type Aux[L<: HList, S1<: String, S2<: String, Out0] = Renamer[L, S1, S2] { type Out = Out0 }

        implicit def hlistReplacer1[T <: HList,S1<: String,S2<: String, U]: Aux[FieldType[Symbol @@ S1,U] :: T, S1, S2, FieldType[Symbol @@ S2,U] :: T] =
          new Renamer[FieldType[Symbol @@ S1,U] :: T, S1, S2] {
            type Out = FieldType[Symbol @@ S2,U] :: T
             def apply(l: FieldType[Symbol @@ S1,U] :: T): FieldType[Symbol @@ S2,U] :: T =
               field[Symbol @@ S2](l.head: U ) :: l.tail
          }

        implicit def hlistReplacer2[H, T <: HList,S1<: String,S2<: String,S3<: String, OutT <: HList](implicit
          ut : Aux[T, S1, S2,  OutT],
        ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,H] :: OutT] =
            new Renamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] {
              type Out =  FieldType[Symbol @@ S3,H] :: OutT
               def apply(l: FieldType[Symbol @@ S3,H] :: T): FieldType[Symbol @@ S3,H] :: OutT =
                 l.head :: ut.apply(l.tail)
            }


        implicit def caseClassToHListGen[S1<: String,S2<: String,S3<: String,H,HT<: HList, T <: HList, OutT <: HList](implicit
          gen: LabelledGeneric.Aux[H,HT],
          ut : FlatRenamer.Aux[HT, S1, S2,  OutT]
        ): Aux[FieldType[Symbol @@ S3,H] :: T, S1, S2, FieldType[Symbol @@ S3,OutT] :: T] =
            new Renamer[FieldType[Symbol @@ S3,H] :: T, S1, S2] {
              type Out = FieldType[Symbol @@ S3,OutT] :: T
              def apply(l: FieldType[Symbol @@ S3,H] :: T): FieldType[Symbol @@ S3,OutT] :: T =
                field[Symbol @@ S3](ut.apply(gen.to(l.head))) :: l.tail
            }
      }

      trait LowerLevel{
      implicit def hlistReplacer3[H,HT<: HList, T <: HList,S1<: String,S2<: String,S3<: String, OutHT <: HList](implicit
        uh : Renamer.Aux[HT, S1, S2,  OutHT]
      ): Renamer.Aux[FieldType[Symbol @@ S3,HT] :: T, S1, S2, FieldType[Symbol @@ S3,OutHT] :: T] =
          new Renamer[FieldType[Symbol @@ S3,HT] :: T, S1, S2] {
            type Out =  FieldType[Symbol @@ S3,OutHT] :: T
            def apply(l: FieldType[Symbol @@ S3,HT] :: T): FieldType[Symbol @@ S3,OutHT] :: T =
              field[Symbol @@ S3](tag[H](uh.apply(l.head))) :: l.tail
          }
      }

    }
  }

  
}
