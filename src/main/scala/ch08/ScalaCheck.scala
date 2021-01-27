package ch08

import Prop._
trait Prop:
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  def &&(p: Prop): Prop = ??? 

object Prop: 
  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???


object Gen: 
  def unit[A](a: => A): Gen[A] = ???


trait Gen[A]: 
  def map[A,B](f: A => B): Gen[B]
  def flatMap[A,B](f: A => Gen[B]): Gen[B]

import State
case class Gen[A](sample: State[RNG, A]):
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???


trait SGen[+A]



//////


case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

