package ch03

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List:
  @tailrec def sum (ints: List[Int]): Int =
    ints match
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)

  @tailrec def product(ds: List[Double]): Double =
    ds match
      case Nil => 1.0
      case Cons(0d, _) => 0d
      case Cons(x, xs) => x * product(xs)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //exercise3.2
  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => ???
      case Cons(_, t) => t

  //exercise3.3
  @tailrec def setHead[A](l: List[A], h: A) =
    l match
      case Nil => ???
      case Cons(_, t) => Cons(h, t)

  //exercise3.4
  @tailrec def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match
        case Nil => Nil
        case Cons(_, t) => drop(t, n-1)

  //exercise3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l

  //dotty only
  val xs: List[Int] = List(1, 2, 3, 5)
  val ex1 = dropWhile(xs, x => x < 4)

  def foldRight[A, B](as: List[A], z: B, f: (A, B) => B): B =
    as match
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z, f))

  //dotty only
  val ex2 = foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0, _ + _)
