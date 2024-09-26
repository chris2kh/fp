trait Option[+A]

case class Some[A](x: A) extends Option[A]
case object None extends Option[Nothing]

trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
    def unit[A](a : A): F[A]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = 
        flatMap(fa)(x => unit(f(x)))
    
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
        flatMap(fa)(a => map(fb)(b => f(a,b))) 
    
    def sequence[A](mas: List[F[A]]): F[List[A]] = mas match
        case Nil => unit(Nil)
        case (h::t) => flatMap(h)(a => map(sequence(t))(as => a::as))
     
    
    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
        sequence(as map f)

    def replicateM[A](n: Int)(ma: F[A]): F[List[A]] =
        sequence(List.fill(n)(ma))

    def filterM[A](xs: List[A])(f: A => F[Boolean]): F[List[A]] =
        val fxs = unit(xs)
        val fbools = traverse(xs)(f)
        val ftuples = map2(fxs, fbools)((as , bools) => as zip bools)
        map(ftuples)(tuples => tuples filter (_._2) map (_._1))
    
    def filterM2[A](xs: List[A])(f: A => F[Boolean]): F[List[A]] =
        flatMap(traverse(xs)(f))(bools =>
                unit(xs zip bools filter (_._2) map (_._1)))

    def compose[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] = a =>
        flatMap(f(a))(g)

    def flatMap2[A,B](ma : F[A])(f: A => F[B]): F[B] =
        compose((x: F[A]) => x)(f)(ma)
    
    def join[A](mma : F[F[A]]): F[A] =
        flatMap(mma)((x: F[A]) => x)

    def flatMap_joinMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
        join(map(ma)(f))

    def compose_joinMap[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] = a =>
        join(map(f(a))(g))
}

val monadOption = new Monad[Option] {
    def unit[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match
        case None => None
        case Some(x) => f(x)
    
}

val monadList = new Monad[List] {
    def unit[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = 
        @annotation.tailrec
        def helper(fa: List[A])(k: List[B] => List[B]): List[B] => List[B] =  fa match
            case Nil => k
            case (h::t) => helper(t)(acc => k(f(h)++ acc))

        helper(fa)( (acc: List[B]) => acc)(Nil: List[B])
}

val monadStream = new Monad[Stream] {
    def unit[A](a: A): Stream[A] = Stream(a)
    def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
        if (fa.isEmpty) then Stream.empty
        else f(fa.head) ++ flatMap(fa.tail)(f)
}

case class Id[A](value : A) {
    def flatMap[B](f: A => Id[B]): Id[B] = 
        monadIdentity.flatMap(this)(f)
    def map[B](f: A => B): Id[B] = monadIdentity.map(this)(f)
}

val monadIdentity = new Monad[Id] {
    def unit[A](a: A): Id[A] = Id(a)
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = 
        f(fa.value)
}

def hello(): Id[String] = for {
    a <- Id("hello")
    b <- Id("monad!")
    } yield a + b

case class Reader[R,A](run: R => A)


object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]}) #f] {
        def unit[A](a: A): Reader[R, A] = Reader(r => a)
        def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(
            r =>
                val a = fa.run(r)
                f(a).run(r)
        )
    }

    def getReader[R] : Reader[R,R] = Reader((r: R) => r)

    def setReader[A,R](r: R): Reader[R,R] = Reader((_:R) => r) 
}