trait Option[+A] {
    def map[B](f : A => B): Option[B] =  // needs match
        this match
            case None => None
            case Some(x) => Some(f(x))
        
    def getOrElse[B >: A](default: => B):B = // needs match
        this match
            case None => default
            case Some(x) => x

    def flatMap[B](f : A => Option[B]): Option[B] =
        this.map(f).getOrElse(None)
        
        
    def orElse[B>: A](ob: => Option[B]):Option[B] =
        this.map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
       this.flatMap(x => if (f(x)) then Some(x) else None)

    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) then None else Some(xs.sum/xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
        mean(xs).flatMap( m => mean(xs.map( x => Math.pow(x- m,2))))

    def lift[A, B](f: A =>B): Option[A] => Option[B] = _ map f     

    def map2[A, B, C](ma: Option[A], mb: Option[B])(f: (A,B) => C): Option[C] = {
        ma.flatMap(a => mb.map(b => f(a,b) ))
    }

    def sequence[A](xs: List[Option[A]]): Option[List[A]] = xs match
        case Nil => Some(Nil)
        case (h::t) => map2(h,sequence(t))( (a,b) => a::b) 

    def inefficientTraverse[A, B](xs : List[A])(f : A => Option[B]): Option[List[B]] =
        sequence(xs map f)

    def efficientTraverse[A, B](xs : List[A])(f : A => Option[B]): Option[List[B]] =
        xs match
            case Nil => Some(Nil)
            case h :: t => map2(f(h), efficientTraverse(t)(f))((a,b) => a::b)

    def sequence2[A,B](xs: List[Option[A]]): Option[List[A]] = 
        efficientTraverse(xs)( x => x)
        
}

case class Some[A](x: A) extends Option[A]
case object None extends Option[Nothing]

trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match
        case Left(x) => Left(x)
        case Right(x) => Right(f(x))
    
    def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] = this match
        case Left(x) => Left(x)
        case Right(x) => f(x)
    
    def orElse[EE >: E, B >:A](obj: Either[EE, B]): Either[EE, B] = this match
        case Left(_) => obj
        case x      => x

    def map2[EE >: E,B,C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
        this.flatMap( a => b.map(bb => f(a,bb) ))

    def sequence[E,A](xs : List[Either[E,A]]) : Either[E, List[A]] = xs match
        case Nil => Right(Nil)
        case h:: t => h.map2(sequence(t))((a,b) => a :: b) 
    
    def traverse[E, A, B](xs : List[A])(f : A => Either[E,B]): Either[E, List[B]] = xs match
        case Nil => Right(Nil)
        case h :: t => f(h).map2(traverse(t)(f))( (a,b) => a :: b)
    

}

case class Left[+E](e:E) extends Either[E, Nothing]
case class Right[+A](x: A) extends Either[Nothing, A]
