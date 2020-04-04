package ch02

object Curry:
  def curry[A, B, C](f: (A,  B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  // test
  def f(a: Int, b: Int): Int = a + b
  def g(a: Int)(b: Int): Int = a + b
  def h(a: Int): Int = a / 2
  def i(a: Int): Int = a + 2
  require(curry(f)(1)(1) == f(1, 1))
  require(uncurry(g)(1, 1) == g(1)(1))
  require(compose(h, i)(0) == compose(h, i)(0))
  // compose(h, i => 0)
