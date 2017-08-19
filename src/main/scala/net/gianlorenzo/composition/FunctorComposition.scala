package net.gianlorenzo.composition

trait Functor[F[_]]{
  def fmap[A, B](f: A => B)(fa: F[A]): F[B]
}

object Functor {
  def compose[F[_] : Functor, G[_] : Functor]: Functor[({type c[T] = F[G[T]]})#c] = new Functor[({type c[T] = F[G[T]]})#c] {
    def ff = implicitly[Functor[F]]
    def gg = implicitly[Functor[G]]
    override def fmap[A, B](f: (A) => B)(fa: F[G[A]]): F[G[B]] = ff.fmap[G[A], G[B]](ga => gg.fmap[A, B](f)(ga))(fa)
  }
}

object FunctorComposition {

  implicit val listInstance = new Functor[({type l[A] = List[A]})#l] {
    override def fmap[A, B](f: (A) => B)(fa: List[A]): List[B] = fa.map(f)
  }

  implicit val optionInstance = new Functor[({type o[A] = Option[A]})#o] {
    override def fmap[A, B](f: (A) => B)(fa: Option[A]): Option[B] = fa.map(f)
  }

  val listOptionInstance = new Functor[({type h[A]=List[Option[A]]})#h]{
    override def fmap[A, B](f: (A) => B)(fa: List[Option[A]]): List[Option[B]] = fa.map(o => o.map(f))
  }

  def main(args: Array[String]): Unit = {

    val listOptionCompose = Functor.compose[List, Option]
    val testListOptions = List(None, None, Some(1))
    case class InterestingWrapper(t: String)
    val testFunc: Int => InterestingWrapper = n => InterestingWrapper(s"${n + 1}")

    println("input:")
    println(testListOptions)
    println("instance:")
    println(listOptionInstance.fmap[Int, InterestingWrapper](testFunc)(testListOptions))
    println("compose:")
    println(listOptionCompose.fmap[Int, InterestingWrapper](testFunc)(testListOptions))
  }
}
