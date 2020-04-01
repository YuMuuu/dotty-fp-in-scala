package ch02
import scala.annotation.tailrec

object Sort:
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    @tailrec def loop(i: Int): Boolean =
      if (i >= as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else loop(i +1)
    loop(0)
