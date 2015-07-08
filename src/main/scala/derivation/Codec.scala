package derivation

import shapeless._

import scala.util.Try

trait Codec[T] {

  def serialize(t: T): String

  def deserialize(s: String): Option[T]
}

object syntax {

  implicit class CodecOps[T](t: T)(implicit codec: Codec[T]) {
    def serialize(): String = codec.serialize(t)
  }

  //implicit def conversion[T](t: T): CodecOps[T] = new CodecOps[T](t)
}

object Codec {

  def apply[T](implicit c: Codec[T]): Codec[T] = c

  implicit def intCodec: Codec[Int] = new Codec[Int] {
    override def serialize(t: Int): String = t.toString

    override def deserialize(s: String): Option[Int] = Try(s.toInt).toOption
  }

  implicit def stringCodec: Codec[String] = new Codec[String] {

    override def serialize(t: String): String = t

    override def deserialize(s: String): Option[String] = Some(s)
  }

  implicit def genericCodec[T, R](implicit gen: Generic.Aux[T, R],
                                  codecRepr: Lazy[Codec[R]]): Codec[T] = new Codec[T] {
    override def serialize(t: T): String = codecRepr.value.serialize(gen.to(t))

    override def deserialize(s: String): Option[T] = codecRepr.value.deserialize(s).map(gen.from)
  }

  implicit val codecHNil: Codec[HNil] = new Codec[HNil] {
    override def serialize(t: HNil): String = "."

    override def deserialize(s: String): Option[HNil] = s match {
      case "." => Some(HNil)
      case _ => None
    }
  }

  implicit val codecCNil: Codec[CNil] = new Codec[CNil] {
    override def serialize(t: CNil): String = ???

    override def deserialize(s: String): Option[CNil] = ???
  }

  implicit def codecHCons[H, T <: HList]
  (implicit codecH: Lazy[Codec[H]],
   codecT: Lazy[Codec[T]]): Codec[H :: T] = new Codec[H :: T] {
    override def serialize(t: H :: T): String = "(" + codecH.value.serialize(t.head) + ", " +
      codecT.value.serialize(t.tail) + ")"

    override def deserialize(s: String): Option[H :: T] = {
      val Pattern = """\((.+?), (.+?)\)""".r
      for {
        Pattern(head, tail) <- Option(s)
        h <- codecH.value.deserialize(head)
        t <- codecT.value.deserialize(tail)
      } yield h :: t
    }
  }

  implicit def codeCCons[H, T <: Coproduct]
  (implicit
     codecH: Lazy[Codec[H]],
     codecT: Lazy[Codec[T]]
    ): Codec[H :+: T] =
      new Codec[H :+: T] {
        def serialize(x: H :+: T): String = x match {
          case Inl(head) => "Left{" + codecH.value.serialize(head) + "}"
          case Inr(tail) => "Right{" + codecT.value.serialize(tail) + "}"
        }

        override def deserialize(s: String): Option[H :+: T] = {
          val LeftPattern = """Left\{(.+)\}""".r
          val RightPattern = """Right\{(.+)\}""".r
          s match {
            case LeftPattern(left) => codecH.value.deserialize(left).map(x => Inl(x))
            case RightPattern(right) => codecT.value.deserialize(right).map(y => Inr(y))
            case _ => None
          }
        }
      }

}

sealed trait TeamMember

case class Todd(breakfast: String, waketime: Int) extends TeamMember
case class Soila(lunch: String, bedtime: Int) extends TeamMember
case class Joyesh(naptime: Int) extends TeamMember


case class TestClass(i: Int, s: String)

object Test extends App {
  import syntax._


  def serialize[T](t: T)(implicit codec: Codec[T]): String = codec.serialize(t)
  def deserialize[T](s: String)(implicit codec: Codec[T]): Option[T] = codec.deserialize(s)

//  val codec = Codec[Int]
//  val codecStr = Codec[String]
//  Generic[TestClass]
//  Codec[HNil]
//  Codec[String :: HNil]
//  Codec[Int :: String :: HNil]
//  val codec1 = Codec[TestClass]
//  val codecTeam = Codec[TeamMember]

  val y = deserialize[TestClass](serialize(TestClass(1, "cat")))
  println(y)
  println(deserialize[TestClass]("DAGs"))

  val todd: TeamMember = Joyesh(12)
  val sTodd =serialize(todd)
  println (s"sTodd=$sTodd")
  val t = deserialize[TeamMember](serialize(Todd("yogurt", 7):TeamMember))
  println(t)
  println(Soila("burger", 12).serialize)

  //assert(codec.deserialize(codec.serialize(5)) == Some(5))

}

