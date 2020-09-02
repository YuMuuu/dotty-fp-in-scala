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
  
      
    


