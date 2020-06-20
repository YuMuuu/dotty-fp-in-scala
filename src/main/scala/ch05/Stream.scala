
import Stream._
enum Stream[+A]:
  case Empty extends Stream[Nothing]
  case Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  // def headOption: Option[A] = this match
  //   case Empty => None
  //   case Cons(h, t) => Some(h())

  //exercise5.1
  def toList: List[A] = 
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match 
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc

    go(this, List()).reverse

  //exercise5.2  
  def take(n: Int): Stream[A] = this match 
    case Cons(h, t) if n > 1 => cons(h(), t().take(n -1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match 
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this

  //exercise5.3
  // def takeWhile(p: A => Boolean): Stream[A] = this match
  //   case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
  //   case _ => empty

  def foldRight[B](z: => B, f:(A, => B) => B): B = this match 
    case Cons(h, t) => f(h(), t().foldRight(z, f))
    case _ => z
    
  def exists(p: A => Boolean): Boolean = foldRight(false, (a , b) => p(a) || b)

  //exercise5.4
  def forAll(p: A => Boolean): Boolean =  foldRight(true, (a, b) => p(a) && b)

  //exercise5.5
  def takeWhile(p: A => Boolean): Stream[A] = 
    foldRight(empty[A], (a, b) => if (p(a)) cons(a, b) else empty)

  //exercise5.6
  def headOption: Option[A] = foldRight(None: Option[A], (a, _) => Some(a))  
  
  //exercise5.7
  def map[B](p: A => B): Stream[B] = 
    foldRight(empty[B], (h, t) => cons(p(h), t))

  def filter(p: A => Boolean): Stream[A] = 
    foldRight(empty[A], (h, t) => if(p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] = 
    foldRight(s, (h, t) => cons(h, t))

  def flatMap[B](p: A => Stream[B]): Stream[B] = 
    foldRight(empty[B], (h, t) => p(h).append(t))
  

object Stream:
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: Stream[A] = Empty
  
  def apply[A](as: A*): Stream[A] = 
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  
  //exercise5.8
  def constant[A](a: A): Stream[A] =  
    // lazy val tail: Stream[A] = Cons(() => a, () => tail)
    // tail
    cons(a, constant(a))

  //exercise5.9
  // def from(n: Int):Stream[Int] = cons(n, from(n + 1))

  //exercise5.10
  // def fib():Stream[Int] = 
  //   def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0+f1))
    
  //   go(0, 1)  
  
  //exercise5.11
  def unfold[A, S](z: S, f: S => Option[(A, S)]): Stream[A] =
    f(z) match 
      case Some(h, s) => cons(h, unfold(s, f))
      case None => empty  

  //exericise5.12
  def from(n: Int): Stream[Int] = 
    unfold[Int, Int](n, m => Some((_: Int, m: Int)))

