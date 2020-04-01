package ch02
import scala.annotation.tailrec

object FibonacciNumber:
  def fib(n: Int): Int =
    @tailrec def loop(n: (Int, Int), max: Int): Int =
      val (x, y) = n
      if (max <= 0 ) x
      else loop((y, x+y), max - 1)
    loop((0, 1), n)
