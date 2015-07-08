package derivation

import shapeless._

import scala.util.Try

trait Codec[T] {

  def serialize(t: T): String

  def deserialize(s: String): Option[T]
}

object Codec {
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
      if (s.startsWith("(") && s.endsWith(")")) {
        val str = s.stripPrefix("(").stripSuffix(")")
        val index = str.indexOf(", ")
        if (index >= 0) {
          val head = str.substring(0, index)
          val tail = str.substring(index + 2)

          for {
            h <- codecH.value.deserialize(head)
            t <- codecT.value.deserialize(tail)
          } yield h :: t
        }
        else {
          None
        }
      }
      else None


    }
  }
}


case class TestClass(i: Int, s: String)

object Test extends App {
  val codec = implicitly[Codec[Int]]
  val codecStr = implicitly[Codec[String]]
  implicitly[Generic[TestClass]]
  implicitly[Codec[HNil]]
  implicitly[Codec[String :: HNil]]
  implicitly[Codec[Int :: String :: HNil]]
  val codec1 = implicitly[Codec[TestClass]]

  val y = codec1.deserialize(codec1.serialize(TestClass(1, "cat")))
  println(codec1.deserialize("DAGs"))
  println(y)

  assert(codec.deserialize(codec.serialize(5)) == Some(5))

}

