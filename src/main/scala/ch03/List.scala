package ch03
import scala.annotation.tailrec

enum List[+A]:
  case Nil 
  case Cons[+A](head: A, tail: List[A]) extends List[A] 

object List:
  def sum (ints: List[Int]): Int =
    ints match
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)

  def product(ds: List[Double]): Double =
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
  def setHead[A](l: List[A], h: A) =
    l match
      case Nil => ???
      case Cons(_, t) => Cons(h, t)

  //exercise3.4
  def drop[A](l: List[A], n: Int): List[A] =
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


  val xs: List[Int] = List(1, 2, 3, 5)
  val ex1 = dropWhile(xs, x => x < 4)

  // def foldRight[A, B](as: List[A], z: B, f: (A, B) => B): B =
  //   as match
  //     case Nil => z
  //     case Cons(x, xs) => f(x, foldRight(xs, z, f))
  //extension methods
  def [A, B](as: List[A]).foldRight(z: B, f: (A, B) => B): B =
    as match
      case Nil => z
      case Cons(x, xs) => f(x, as.foldRight(z, f))

  val ex2 = Cons(1, Cons(2, Cons(3, Nil))).foldRight(0, _ + _)


  //exercise3.9
  def length[A](as: List[A]): Int =
    as.foldRight(0, (_,cnt) => cnt + 1)


  //exercise3.10
  // def foldLeft[A, B](as: List[A], z: B, f: (B, A) => B): B =
  //   as match
  //     case Nil => z
  //     case Cons(x, xs) => foldLeft(xs, f(z, x), f)
  //extension methods
  def [A, B](l: List[A]).foldLeft(z: B, f: (B, A) => B): B =
    l match
      case Nil => z
      case Cons(x, xs) => xs.foldLeft(f(z, x), f)


  //exercise3.11
  def _sum(ints: List[Int]): Int =
    ints.foldLeft(0, _ + _)

  def _product(ds: List[Double]): Double =
    ds.foldLeft(1, _ * _)

  def _length[A](l: List[A]): Int =
    l.foldLeft(0, (acc, _) => acc + 1)

  //exercise3.12
  // def reverse[A](l: List[A]): List[A] =
    // l.foldLeft(List[A](), (acc, c) => Cons(c, acc))
  //extension methods
  def [A](l: List[A]).reverse: List[A] =
    l.foldLeft(Nil: List[A] , (acc, c) => Cons(c, acc))

  //exercise3.13
  // 答え見た
  def foldRightLeft[A,B](l: List[A], z: B, f: (A,B) => B): B =
    l.foldLeft((b: B) => b, (e, a) => b => e(f(a, b)))(z)
  def foldLeftRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    l.foldRight((b: B) => b, (a, e) => b => e(f(b, a)))(z)

  //exercise3.14
  // def append[A](l: List[A], z: List[A]) = l.foldRight(z, Cons(_, _))
  def [A](l: List[A]).append(z: List[A]) = l.foldRight(z, Cons(_, _))
  def [A](l: List[A]).append2(z: List[A]): List[A] = 
    l match 
      case Nil => z
      case Cons(h, t) => Cons(h, t.append2(z))

 //exercise3.15
  // def flatten[A](ll: List[List[A]]): List[A] = ll.foldRight(Nil: List[A], (l, h) => l.append(h))
  def [A](ll: List[List[A]]).flatten: List[A] = ll.foldRight(Nil: List[A], (l, h) => l.append(h))

  //exercise3.16
  // def allIncrement(l: List[Int]): List[Int] = l.foldRight(Nil: List[Int], (h, t) => Cons(h + 1, t))
  def (l: List[Int]).allIncrement: List[Int] = l.foldRight(Nil: List[Int], (h, t) => Cons(h + 1, t))

  //exercise3.17
  //3.16と同じなので省略

  //exercise3.18
  // def map[A, B](as: List[A], f: A => B): List[B] =
  //   as.foldRight(Nil: List[B], (h, t) => Cons(f(h), t)as.foldRight(Nil: List[B], (h, t) => Cons(f(h), t)))
  def [A, B](as: List[A]).map(f: A => B): List[B] =
    as.foldRight(Nil: List[B], (h, t) => Cons(f(h), t))

  def [A, B](as: List[A]).map2(f: A => B): List[B] = 
    as match 
      case Nil        => Nil
      case Cons(h, t) => Cons(f(h), t.map2(f))
   

  //exercise3.19
  // def filter[A](as: List[A], f: A => Boolean): List[A] =
  //   as.foldRight(Nil:List[A], (h, t) => if (f(h)) Cons(h,t) else t)
  def [A](as: List[A]).filter(f: A => Boolean): List[A] =
    as.foldRight(Nil:List[A], (h, t) => if f(h) then Cons(h,t) else t)
  
  def [A](as: List[A]).filter2(f: A => Boolean): List[A] = 
    as match
      case Nil => Nil
      case Cons(h, t) if(!f(h)) => t 
      case Cons(h, t) if(f(h)) => Cons(h, t.filter2(f))  
  
    

  //exercise3.20
  // def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as.map(f).flatten
  def [A, B](as: List[A]).flatMap(f: A => List[B]): List[B] = as.map(f).flatten

  def [A, B](as: List[A]).flatMap2(f: A => List[B]): List[B] = 
    as.map(f).foldRight(Nil: List[B], (l, h) => l.foldRight(h, Cons(_, _)))

  // def [A, B](as: List[A]).flatMap3(f: A => List[B]): List[B] = 
  //   as.map(f) match
  //     case Nil => Nil
  //     case Cons(h, t): List[List[B]] => 
  //       // h.append2(t.flatten)
  //       t match
  //         case Nil => Nil
  //         case Cons(Nil, css) => ???
  //         case Cons((), css) => Cons()
         


  //exercise3.21
  // def filter[A](as: List[A], f: A => Boolean): List[A] = as.flatMap(a => if (f(a)) List(a) else Nil)

  //exercise3.22
  def zipWithSum(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2,zipWithSum(t1,t2))

  //exercise3.23
  // 拡張関数だけ
  def [A, B, C](la: List[A]).zipWith(lb: List[B], f:(A, B) => C): List[C] =
    (la, lb) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), t1.zipWith(t2, f))

  //exercise3.24
  //答え見た
  @tailrec def [A](l: List[A]).startsWith(prefix: List[A]): Boolean =
    (l, prefix) match
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => t1.startsWith(t2)
      case _ => false

  @tailrec def [A](sup: List[A]).hasSubsequence(sub: List[A]): Boolean =
    sup match 
      case Nil => sub == Nil
      case _ if sup.startsWith(sub) => true
      case Cons(h, t) => sup.hasSubsequence(sub)
    
