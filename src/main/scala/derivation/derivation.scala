/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package derivation

import shapeless._

object repr extends ShowReprDefns {

  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal

  val felix: Animal = Cat("Felix", 1)
  val tigger: Animal = Dog("Tigger", 2)
}

object equalManual {
  // Cats/Algebra Eq
  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
  }

  object Eq {
    implicit val eqInt: Eq[Int] =
      new Eq[Int] {
        def eqv(x: Int, y: Int): Boolean = x == y
      }

    implicit val eqString: Eq[String] =
      new Eq[String] {
        def eqv(x: String, y: String): Boolean = x == y
      }
  }

  implicit class EqOps[T](x: T)(implicit eqT: Eq[T]) {
    def ===(y: T): Boolean = eqT.eqv(x, y)
  }

  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal

  object Animal {
    implicit val eqAnimal: Eq[Animal] =
      new Eq[Animal] {
        def eqv(x: Animal, y: Animal): Boolean =
          (x, y) match {
            case (x: Cat, y: Cat) => x === y
            case (x: Dog, y: Dog) => x === y
            case _ => false
          }
      }
  }

  object Cat {
    implicit val eqCat: Eq[Cat] =
      new Eq[Cat] {
        def eqv(x: Cat, y: Cat): Boolean =
          x.name === y.name && x.fish === y.fish
      }
  }

  object Dog {
    implicit val eqDog: Eq[Dog] =
      new Eq[Dog] {
        def eqv(x: Dog, y: Dog): Boolean =
          x.name === y.name && x.bones === y.bones
      }
  }

  val felix: Animal = Cat("Felix", 1)
  val tigger: Animal = Dog("Tigger", 2)
}

object equal {
  // Cats/Algebra Eq
  trait Eq[T] {
    def eqv(x: T, y: T): Boolean
  }

  object Eq {
    implicit val eqInt: Eq[Int] =
      new Eq[Int] {
        def eqv(x: Int, y: Int): Boolean = x == y
      }

    implicit val eqString: Eq[String] =
      new Eq[String] {
        def eqv(x: String, y: String): Boolean = x == y
      }

    implicit def eqGeneric[T, R]
      (implicit
        gen: Generic.Aux[T, R],
        eqRepr: Lazy[Eq[R]]
      ): Eq[T] =
        new Eq[T] {
          def eqv(x: T, y: T): Boolean =
            eqRepr.value.eqv(gen.to(x), gen.to(y))
        }

    // Base case for products
    implicit val eqHNil: Eq[HNil] = new Eq[HNil] {
      def eqv(x: HNil, y: HNil): Boolean = true
    }

    // Induction step for products
    implicit def eqHCons[H, T <: HList]
      (implicit
        eqH: Lazy[Eq[H]],
        eqT: Lazy[Eq[T]]
      ): Eq[H :: T] =
        new Eq[H :: T] {
          def eqv(x: H :: T, y: H :: T): Boolean =
            eqH.value.eqv(x.head, y.head) && eqT.value.eqv(x.tail, y.tail)
        }

    // Base case for coproducts
    implicit val eqCNil: Eq[CNil] = new Eq[CNil] {
      def eqv(x: CNil, y: CNil): Boolean = true
    }

    // Induction step for products
    implicit def eqCNCons[H, T <: Coproduct]
      (implicit
        eqH: Lazy[Eq[H]],
        eqT: Lazy[Eq[T]]
      ): Eq[H :+: T] =
        new Eq[H :+: T] {
          def eqv(x: H :+: T, y: H :+: T): Boolean =
            (x, y) match {
              case (Inl(xh), Inl(yh)) => eqH.value.eqv(xh, yh)
              case (Inr(xt), Inr(yt)) => eqT.value.eqv(xt, yt)
              case _ => false
            }
        }
  }

  implicit class EqOps[T](x: T)(implicit eqT: Eq[T]) {
    def ===(y: T): Boolean = eqT.eqv(x, y)
  }

  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal

  val felix: Animal = Cat("Felix", 1)
  val tigger: Animal = Dog("Tigger", 2)
}

object ordering {
  // Cats/Algebra Order
  trait Order[T] {
    def compare(x: T, y: T): Int
  }

  object Order {
    implicit val ordInt: Order[Int] =
      new Order[Int] {
        def compare(x: Int, y: Int): Int = x-y
      }

    implicit val ordString: Order[String] =
      new Order[String] {
        def compare(x: String, y: String): Int = x compare y
      }

    implicit def ordGeneric[T, R]
      (implicit
        gen: Generic.Aux[T, R],
        ordRepr: Lazy[Order[R]]
      ): Order[T] =
        new Order[T] {
          def compare(x: T, y: T): Int =
            ordRepr.value.compare(gen.to(x), gen.to(y))
        }

    // Base case for products
    implicit val ordHNil: Order[HNil] = new Order[HNil] {
      def compare(x: HNil, y: HNil): Int = 0
    }

    // Induction step for products
    implicit def ordHCons[H, T <: HList]
      (implicit
        ordH: Lazy[Order[H]],
        ordT: Lazy[Order[T]]
      ): Order[H :: T] =
        new Order[H :: T] {
          def compare(x: H :: T, y: H :: T): Int = {
            val cmpH = ordH.value.compare(x.head, y.head)
            if(cmpH != 0) cmpH else ordT.value.compare(x.tail, y.tail)
          }
        }

    // Base case for coproducts
    implicit val ordCNil: Order[CNil] = new Order[CNil] {
      def compare(x: CNil, y: CNil): Int = 0
    }

    // Induction step for products
    implicit def ordCNCons[H, T <: Coproduct]
      (implicit
        ordH: Lazy[Order[H]],
        ordT: Lazy[Order[T]]
      ): Order[H :+: T] =
        new Order[H :+: T] {
          def compare(x: H :+: T, y: H :+: T): Int =
            (x, y) match {
              case (Inl(xh), Inl(yh)) => ordH.value.compare(xh, yh)
              case (Inl(xh), Inr(yt)) => 1
              case (Inr(xh), Inl(yt)) => -1
              case (Inr(xt), Inr(yt)) => ordT.value.compare(xt, yt)
            }
        }
  }

  implicit class OrderOps[T](x: T)(implicit ordT: Order[T]) {
    def compare(y: T): Int = ordT.compare(x, y)
  }

  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal

  val felix: Animal = Cat("Felix", 1)
  val tigger: Animal = Dog("Tigger", 2)
}

object monoid {
  // Cats/Algebra monoid
  trait Monoid[T] {
    def empty: T
    def combine(x: T, y: T): T
  }

  object Monoid {
    def apply[T](implicit monT: Monoid[T]) = monT

    implicit val booleanMonoid: Monoid[Boolean] =
      new Monoid[Boolean] {
        val empty = false
        def combine(x: Boolean, y: Boolean) = x || y
      }

    implicit val intMonoid: Monoid[Int] =
      new Monoid[Int] {
        val empty = 0
        def combine(x: Int, y: Int) = x+y
      }

    implicit val doubleMonoid: Monoid[Double] =
      new Monoid[Double] {
        val empty = 0.0
        def combine(x: Double, y: Double) = x+y
      }

    implicit val stringMonoid: Monoid[String] =
      new Monoid[String] {
        val empty = ""
        def combine(x: String, y: String) = x+y
      }

    implicit def monGeneric[T, R]
      (implicit
        gen: Generic.Aux[T, R],
        monRepr: Lazy[Monoid[R]]
      ): Monoid[T] =
        new Monoid[T] {
          val empty = gen.from(monRepr.value.empty)
          def combine(x: T, y: T): T =
            gen.from(monRepr.value.combine(gen.to(x), gen.to(y)))
        }

    // Base case for products
    implicit val monHNil: Monoid[HNil] = new Monoid[HNil] {
      val empty = HNil
      def combine(x: HNil, y: HNil) = HNil
    }

    // Induction step for products
    implicit def monHCons[H, T <: HList]
      (implicit
        monH: Lazy[Monoid[H]],
        monT: Lazy[Monoid[T]]
      ): Monoid[H :: T] =
        new Monoid[H :: T] {
          val empty = monH.value.empty :: monT.value.empty
          def combine(x: H :: T, y: H :: T) =
            monH.value.combine(x.head, y.head) ::
              monT.value.combine(x.tail, y.tail)
        }
  }

  implicit class MonoidOps[T](x: T)(implicit monT: Monoid[T]) {
    def combine(y: T): T = monT.combine(x, y)
  }

  case class Cat(name: String, fish: Int)
  case class Dog(name: String, bones: Int)

  val felix = Cat("Felix", 1)
  val tigger = Dog("Tigger", 2)
}

object show {
  // Show using low-level infrastructure ...

  import labelled._

  trait Show[T] {
    def show(t: T): String
  }

  object Show {
    implicit val showString: Show[String] = new Show[String] {
      def show(t: String) = t
    }

    implicit val showInt: Show[Int] = new Show[Int] {
      def show(t: Int) = t.toString
    }

    implicit def showList[A](implicit showA: Show[A]): Show[List[A]] = new Show[List[A]] {
      def show(t: List[A]) = t.map(showA.show).mkString("List(", ", ", ")")
    }

    implicit def showGeneric[F, G](implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[Show[G]]): Show[F] =
      new Show[F] {
        def show(f: F) = sg.value.show(gen.to(f))
      }

    implicit def showHNil: Show[HNil] =
      new Show[HNil] {
        def show(p: HNil): String = ""
      }

    implicit def showHCons[K <: Symbol, V, T <: HList]
      (implicit
        key: Witness.Aux[K],
        sv: Lazy[Show[V]],
        st: Lazy[Show[T]]
      ): Show[FieldType[K, V] :: T] =
        new Show[FieldType[K, V] :: T] {
          def show(p: FieldType[K, V] :: T): String = {
            val head = s"${key.value.name} = ${sv.value.show(p.head)}"
            val tail = st.value.show(p.tail)
            if(tail.isEmpty) head else s"$head, $tail"
          }
        }

    implicit def showCNil: Show[CNil] =
      new Show[CNil] {
        def show(p: CNil): String = ""
      }

    implicit def showCCons[K <: Symbol, V, T <: Coproduct]
      (implicit
        key: Witness.Aux[K],
        sv: Lazy[Show[V]],
        st: Lazy[Show[T]]
      ): Show[FieldType[K, V] :+: T] =
        new Show[FieldType[K, V] :+: T] {
          def show(c: FieldType[K, V] :+: T): String =
            c match {
              case Inl(l) => s"${key.value.name}(${sv.value.show(l)})"
              case Inr(r) => st.value.show(r)
            }
        }
  }

  implicit class ShowOps[T](x: T)(implicit showT: Show[T]) {
    def show: String = showT.show(x)
  }

  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal

  val felix = Cat("Felix", 1)
  val tigger = Dog("Tigger", 2)
}

object show2 {
  // Show using TypeClass
  trait Show[T] {
    def show(t: T): String
  }

  object Show extends LabelledTypeClassCompanion[Show] {
    implicit def stringShow: Show[String] = new Show[String] {
      def show(t: String) = t
    }

    implicit def intShow: Show[Int] = new Show[Int] {
      def show(n: Int) = n.toString
    }

    object typeClass extends LabelledTypeClass[Show] {
      def emptyProduct = new Show[HNil] {
        def show(t: HNil) = ""
      }

      def product[F, T <: HList](name: String, sh: Show[F], st: Show[T]) = new Show[F :: T] {
        def show(ft: F :: T) = {
          val head = sh.show(ft.head)
          val tail = st.show(ft.tail)
          if (tail.isEmpty)
            s"$name = $head"
          else
            s"$name = $head, $tail"
        }
      }

      def emptyCoproduct = new Show[CNil] {
        def show(t: CNil) = ""
      }

      def coproduct[L, R <: Coproduct](name: String, sl: => Show[L], sr: => Show[R]) = new Show[L :+: R] {
        def show(lr: L :+: R) = lr match {
          case Inl(l) => s"$name(${sl.show(l)})"
          case Inr(r) => s"${sr.show(r)}"
        }
      }

      def project[F, G](instance: => Show[G], to: F => G, from: G => F) = new Show[F] {
        def show(f: F) = instance.show(to(f))
      }
    }
  }

  implicit class ShowOps[T](x: T)(implicit showT: Show[T]) {
    def show: String = showT.show(x)
  }

  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal

  val felix = Cat("Felix", 1)
  val tigger = Dog("Tigger", 2)
}

object functor {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor extends Functor0 {
    def apply[F[_]](implicit f: Lazy[Functor[F]]): Functor[F] = f.value

    implicit val idFunctor: Functor[Id] =
      new Functor[Id] {
        def map[A, B](a: A)(f: A => B): B = f(a)
      }

    // Induction step for products
    implicit def hcons[F[_]](implicit ihc: IsHCons1[F, Functor, Functor]): Functor[F] =
      new Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] = {
          val (hd, tl) = ihc.unpack(fa)
          ihc.pack((ihc.fh.map(hd)(f), ihc.ft.map(tl)(f)))
        }
      }

    // Induction step for coproducts
    implicit def ccons[F[_]](implicit icc: IsCCons1[F, Functor, Functor]): Functor[F] =
      new Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] =
          icc.pack(icc.unpack(fa).fold(hd => Left(icc.fh.map(hd)(f)), tl => Right(icc.ft.map(tl)(f))))
      }

    implicit def generic[F[_]](implicit gen: Generic1[F, Functor]): Functor[F] =
      new Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] =
          gen.from(gen.fr.map(gen.to(fa))(f))
      }
  }

  trait Functor0 {
    implicit def constFunctor[T]: Functor[Const[T]#λ] =
      new Functor[Const[T]#λ] {
        def map[A, B](t: T)(f: A => B): T = t
      }
  }

  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(fa)(f)
  }

  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  val tree =
    Node(
      Leaf("quux"),
      Node(
        Leaf("foo"),
        Leaf("wibble")
      )
    )
}

object foldable {
  import scala.util.control.TailCalls._

  trait Foldable[F[_]] {
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  }

  object Foldable {
    implicit def apply[F[_]](implicit fr: Lazy[FoldableRec[F]]): Foldable[F] =
      new Foldable[F] {
        def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = fr.value.foldLeft(fa, b)(f).result
      }
  }

  trait FoldableRec[F[_]] {
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): TailRec[B]
  }

  object FoldableRec extends FoldableRec0 {
    def apply[F[_]](implicit F: Lazy[FoldableRec[F]]): FoldableRec[F] = F.value

    implicit val idFoldableRec: FoldableRec[Id] =
      new FoldableRec[Id] {
        def foldLeft[A, B](fa: A, b: B)(f: (B, A) => B) =
          done(f(b, fa))
      }

    implicit def hcons[F[_]](implicit F: IsHCons1[F, FoldableRec, FoldableRec]): FoldableRec[F] =
      new FoldableRec[F] {
        override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B) = {
          val (hd, tl) = F.unpack(fa)
          for {
            h <- F.fh.foldLeft(hd, b)(f)
            t <- F.ft.foldLeft(tl, h)(f)
          } yield t
        }
      }

    implicit def ccons[F[_]](implicit F: IsCCons1[F, FoldableRec, FoldableRec]): FoldableRec[F] =
      new FoldableRec[F] {
        def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B) =
          F.unpack(fa) match {
            case Left(l) =>
              tailcall(F.fh.foldLeft(l, b)(f))
            case Right(r) =>
              tailcall(F.ft.foldLeft(r, b)(f))
          }
      }

    implicit def generic[F[_]](implicit F: Generic1[F, FoldableRec]): FoldableRec[F] =
      new FoldableRec[F] {
        def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B) =
          tailcall(F.fr.foldLeft(F.to(fa), b)(f))
      }
  }

  sealed abstract class FoldableRec0 {
    implicit def constFoldableRec[T]: FoldableRec[Const[T]#λ] =
      new FoldableRec[Const[T]#λ] {
        override def foldLeft[A, B](fa: T, b: B)(f: (B, A) => B) =
          done(b)
      }
  }

  implicit class FoldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]) {
    def foldLeft[B](b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)(f)
  }

  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  val tree =
    Node(
      Leaf("quux"),
      Node(
        Leaf("foo"),
        Leaf("wibble")
      )
    )

  sealed abstract class IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  val list = (1 to 100000).foldLeft(ICons(0, INil())){ (acc, i) => ICons(i, acc) }
}

trait ShowReprDefns {
  def showRepr[T](t: T)(implicit st: ShowRepr[T]): String = st(t)

  trait ShowRepr[T] {
    def apply(t: T): String
  }

  object ShowRepr extends ShowRepr0 {
    implicit val hnilShowRepr: ShowRepr[HNil] =
      new ShowRepr[HNil] {
        def apply(t: HNil): String = "HNil"
      }

    implicit def hconsShowRepr[H, T <: HList]
      (implicit
        sh: Lazy[ShowRepr[H]],
        st: Lazy[ShowRepr[T]]
      ): ShowRepr[H :: T] =
      new ShowRepr[H :: T] {
        def apply(t: H :: T): String = sh.value(t.head)+" :: "+st.value(t.tail)
      }

    implicit val cnilShowRepr: ShowRepr[CNil] =
      new ShowRepr[CNil] {
        def apply(t: CNil): String = "CNil"
      }

    implicit def cconsShowRepr[H, T <: Coproduct]
      (implicit
        sh: Lazy[ShowRepr[H]],
        st: Lazy[ShowRepr[T]]
      ): ShowRepr[H :+: T] =
      new ShowRepr[H :+: T] {
        def apply(t: H :+: T): String =
          t match {
            case Inl(l) => "Inl("+sh.value(l)+")"
            case Inr(r) => "Inr("+st.value(r)+")" 
          }
      }

    implicit def genShowRepr[T, R]
      (implicit
        gen: Generic.Aux[T, R],
        sr: Lazy[ShowRepr[R]]
      ): ShowRepr[T] =
      new ShowRepr[T] {
        def apply(t: T): String = sr.value(gen.to(t))
      }
  }
  
  trait ShowRepr0 {
    implicit def default[T]: ShowRepr[T] =
      new ShowRepr[T] {
        def apply(t: T): String = t.toString
      }
  }
}
