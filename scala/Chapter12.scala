
trait Applicative[F[_]] {
    def unit[A](a : A): F[A]
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
    def apply[A,B](ff: F[A =>B])(fa: F[A]): F[B] =
        map2(ff, fa)(_(_))



    def mapViaApply[A,B](fa: F[A])(f: A => B): F[B] =
        apply(unit(f))(fa)
        
    def map2ViaApply[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
        val pf = apply(unit(f.curried))(fa)
        apply(pf)(fb)

    def applyViaMap2[A,B](ff: F[A =>B])(fa: F[A]): F[B] =
        map2(ff,fa)((f, x) => f(x))

    
    def map[A,B](fa: F[A])(f: A => B): F[B] =
        map2(fa, unit(()))((a, _) => f(a))

    def sequence[A](mas: List[F[A]]): F[List[A]] =
        traverse(mas)(x => x)

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
        as.foldRight(unit(Nil:List[B]))((a, acc) => map2(f(a), acc)(_::_))

    def replicateM[A](n: Int)(ma: F[A]): F[List[A]] =
        sequence(List.fill(n)(ma))

    def filterM[A](xs: List[A])(f: A => F[Boolean]): F[List[A]] =
        val fxs = unit(xs)
        val fbools = traverse(xs)(f)
        val ftuples = map2(fxs, fbools)((as , bools) => as zip bools)
        map(ftuples)(tuples => tuples filter (_._2) map (_._1))

    def product[A,B](fa : F[A], fb: F[B]): F[(A,B)] =
        map2(fa, fb)((_,_))

    def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] =
        apply(apply(map(fa)(f.curried))(fb))(fc)
    
    
    def map4[A,B,C,D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] =
        apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

    def sequenceMap[K, V](entries: Map[K, F[V]]): F[Map[K, V]] =
        entries.foldRight(unit(Map[K,V]()))( (entry, acc) => 
            val key = entry._1
            val fval = entry._2 

            map2(acc,fval)((acc, v) => acc.updated(key, v)))
}

trait Monad[F[_]] {
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

def eitherMonad[E]: Monad[({ type f[x] = Either[E,x]})#f]  =
    new Monad[({ type f[x] = Either[E,x]})#f] {
        def unit[A](a: A): Either[E, A] = Right(a)
        def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match
            case Left(x) => Left(x)
            case Right(a) => f(a)
        
    }

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

def applicativeValidation[E] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    def unit[A](a: A): Validation[E, A] = Success(a)

    def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match
            case (Failure(h1, t1), Failure(h2, t2)) => Failure(h2, t1.appendedAll(t2))
            case (_, Failure(h,t)) => Failure(h, t)
            case (Success(x), Success(y)) => Success(f(x,y))
            case (Failure(h,t), _) => Failure(h, t)
        
}

//def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    //new Applicative[({type f[x] = (F[x], G[x])})#f] {
      //val self = this
      //def unit[A](a: => A) = (self.unit(a), G.unit(a))

      //override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        //(self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
//}

trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
}

case class Id[A](value: A)

val applicativeId = new Applicative[Id] {
    def unit[A](a: A): Id[A] = Id(a)
    def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = 
        unit(f(fa.value, fb.value))

    

}

trait Traverse[F[_]] extends Functor[F] { self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  val idApp = new Applicative[Id] {
    def unit[A](a: => A) = a

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
        unit(f(fa, fb))
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idApp)
}


def listTraverse[A] : Traverse[List] =
    new Traverse[List] {
        override def sequence[G[_]: Applicative, A](fga: List[G[A]])(implicit G: Applicative[G]): G[List[A]] =
            fga.foldRight(G.unit(List()))(G.map2(_,_)(_::_))
    }

def optionTraverse[A]: Traverse[Option] =
    new Traverse[Option] {
        override def sequence[G[_]: Applicative, A](fga: Option[G[A]])(implicit G: Applicative[G]): G[Option[A]] = 
            fga match
                case None => G.unit(None)
                case Some(ga) => G.map(ga)(Some(_))  
        
    }

case class Tree[+A](head: A, tail: List[Tree[A]])

val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_],A,B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }