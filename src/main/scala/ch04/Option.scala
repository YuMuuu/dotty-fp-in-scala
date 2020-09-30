package ch03

enum Option[+A]:
  self =>
    def map[B](f: A => B): Option[B] = self match 
      case Some(v) => Some(f(v))
      case None => None
    
    def flatMap[B](f: A => Option[B]): Option[B] = self match 
      case Some(v) => f(v)
      case None => None
    
    def getOrElse[B >: A](b: => B): B = self match 
      case Some(a) => a
      case None    => b
    

    def orElse[B >: A](ob: => Option[B]): Option[B] = self match 
      case v @ Some(_) => v
      case None        => ob
    

    def filter(f: A => Boolean): Option[A] = self match 
      case v @ Some(a) if f(a) => v
      case _                   => None

  case Some[+A](get: A) extends Option[A]
  case None    
  
      
    
object Option {
  import scala.math._
  //exrcise 4.2
  def mean(xs: Seq[Double]): Option[Double] = 
    // xs.foldLeft(0.0) { (acc, x) => acc + x } match 
    //   case 0.0 => None
    xs match 
      case em:Seq[Double] if em.length == 0 => None
      case _ => Some( xs.foldLeft(0.0) { (acc, x) => acc + x } / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    xs match
      case em:Seq[Double] if em.length == 0 => None 
      case _ => {
        val n = xs.length
        val some = xs.foldLeft(0.0) { (acc, x) => acc + x }

        for 
          mean <- Option.mean(xs)
         yield xs.foldLeft(0.0) { (acc, x) => ( acc + pow(x - mean, 2)) } / n 
      }
  def lift[A, B](f: A => B): Option[A] => Option[B] =  _ map f   

  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  //4.3
  def map2[A, B, C](a: Option[A], b: Option[B], f: (A, B) => C): Option[C] = 
    // a flatMap (aa =>
    //   b map (bb => 
    //     f(aa, bb))
    //   )
    for 
      aa <- a
      bb <- b
    yield f(aa, bb)

  //4.4
  //Listの実装が間違えているのでこれは動かない
  // def sequence[A](a: List[Option[A]]): Option[List[A]] = 
  //   a.foldRight(Some(Nil): Option[List[A]])((r, acc) =>
  //     for 
  //       l <- acc
  //       v <- r
  //     yield l ++ List(v))
}


