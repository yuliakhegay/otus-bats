package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => point(f(a)))

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(a => a)
}

object Monad {

  def apply[F[_]](implicit ev: Monad[F]): Monad[F] = ev

  implicit val optMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    override def point[A](a: A): Option[A] = Option(a)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    override def point[A](a: A): List[A] = List(a)
  }

    implicit val setMonad: Monad[Set] = new Monad[Set] {
      override def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)
      override def point[A](a: A): Set[A] = Set(a)

    }

  }