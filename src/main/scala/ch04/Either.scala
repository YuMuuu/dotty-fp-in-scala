enum Either[+E, +A]:
  self =>
    def map[B](f: A => B): Either[E, B] = 
      self match 
        case err@ Left(_) => err
        case Right(v) => Right(f(v))

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = 
      self match 
        case Right(a) => f(a)
        case Left => this.asInstanceOf[Either[EE, B]]
    
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = 
      self match 
        case Right(a) => this
        case Left    => b
    
    def map2[EE >: E, B, C](b: Either[EE, B], f: (A, B) => C): Either[EE, C] = 
      for
        a <- self
        b1 <- b
      yield f(a,b1)

    // import scala.collection.imutable.{List => SList} 
    import scala.collection.immutable.{List => SList}
    def traverse[E,A,B](es: SList[A], f: A => Either[E, B]): Either[E, SList[B]] = 
      es match 
        case Nil => Right(Nil)
        case h::t => f(h).map2(traverse(t, f), _ :: _)
      // es.foldRight(Right(Nil): Either[E, List[B]])((a, b) => f(a).map2(b, _ :: _))
        
          

    
  case Left[+E](value: E) extends Either[E, Nothing]
  case Right[+A](value: A) extends Either[Nothing, A]