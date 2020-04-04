package ch03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], rigth: Tree[A]) extends Tree[A]

//topレベルに関数を定義出来るので object Tree{} とはしない

//exercise3.25
// def [A](tr: Tree[A]).size: Int =
//   tr match
//     case Leaf(_) => 1
//     case Branch(a, b) => 1 + size(a) + size(b)

//exercise3.26
// def (tr: Tree[Int]).maximum: Int =
//   tr match
//     case Leaf(n) => n
//     case Branch(l, r) => l.maximum max r.maximum

//exercise3.27
// def (tr: Tree[Int]).depth: Int =
//   tr match
//     case Leaf(_) => 0
//     case Branch(l, r) => 1 + l.depth max r.depth

//exercise3.28
// def [A, B](tr: Tree[A]).map(f: A => B): Tree[B] =
//   tr match
//     case Leaf(x) => Leaf(f(x))
//     case Branch(l, r) => Branch(l.map(f), r.map(f))

//exercise3.29
def [A, B](tr: Tree[A]).fold(f1: A => B, f2: (B, B) => B): B =
  tr match
    case Leaf(x) => f1(x)
    case Branch(l: Tree[A], r: Tree[A]) => f2(l.fold(f1, f2), r.fold(f1, f2))

def [A](tr: Tree[A]).size: Int = tr.fold(a => 1, 1 + _ + _)
def (tr: Tree[Int]).maximum: Int = tr.fold(a => a, _ max _)
def [A](tr: Tree[A]).depth: Int = tr.fold(a => 0, 1 + _ max _)
def [A, B](tr: Tree[A]).map(f: A => B): Tree[B] = tr.fold(a => Leaf(f(a)): Tree[B], Branch(_, _))
