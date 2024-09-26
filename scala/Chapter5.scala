sealed trait Stream[+A] {
    def toList : List[A] = this match
        case Empty => Nil
        case Cons(x,xs) => x() :: xs().toList

    def take(n : Int): Stream[A] = {
        n match
            case 0 => Stream.empty
            case _ => this match
                case Empty => Stream.empty
                case Cons(x,xs) => Stream.cons(x(), xs().take(n-1))
    }

    def drop(n : Int): Stream[A] = {
        n match
            case 0 => this
            case _ => this match
                case Empty => Stream.empty
                case Cons(x,xs) => xs().drop(n-1)
    }
    
    def foldRight[B](z: => B)(f : (A, => B) => B): B = {
        this match
            case Empty => z
            case Cons(x,xs) => f(x(), xs().foldRight(z)(f))
    }

    def exists(f: A => Boolean): Boolean =
        this.foldRight(false)((a,b) => f(a) || b) 

    def forAll(f: A => Boolean): Boolean = 
        this.foldRight(true)((a,b) => f(a) && b)

    def takeWhile(f: A => Boolean): Stream[A] = {
        this.foldRight(Stream.empty)((a,b) => if f(a) then Stream.cons(a, b.takeWhile(f)) else Stream.empty)
    }

    def headOption : Option[A] =
        this.foldRight(None: Option[A])((a,_) => Some(a))

    def map[B](f: A => B): Stream[B] =
        this.foldRight(Empty: Stream[B])((a,b) => Stream.cons(f(a),b))

    def filter(p : A => Boolean): Stream[A] =
        this.foldRight(Empty: Stream[A])((a,b) => if p(a) then Stream.cons(a,b) else b)
        
    def append[AA >: A](stream2: Stream[AA]): Stream[AA] =
        this.foldRight(stream2)((a,b) => Stream.cons(a,b))

    def flatMap[B](f : A => Stream[B]):Stream[B] =
        this.foldRight(Empty: Stream[B])( (a,b) => f(a).append(b))

    def mapUnfold[B](f: A => B): Stream[B] =
        Stream.unfold(this)(x => x match
            case Empty => None
            case Cons(h,t) => Some(f(h()), t()))
    
    def takeUnfold(n : Int): Stream[A] =
        Stream.unfold((n,this))((n, xs) => 
            n match
                case 0 => None
                case n => xs match
                    case Empty => None
                    case Cons(h,t) => Some(h(), (n-1, t()) ))
    
    def takeWhileUnfold(f: A => Boolean): Stream[A] = {
        Stream.unfold((f,this))( (f, xs) => xs match
            case Cons(h, t) if f(h()) => Some(h(), (f, t()))
            case _ => None
        )
    }

    def zipWithUnfold[B,C](s2:Stream[B])(f: A => B => C):Stream[C] = 
        Stream.unfold((this, s2))( (s1,s2) =>
            val tuple = (s1, s2)
            tuple match
                case (Cons(s1x,s1xs), Cons(s2x,s2xs)) => Some( f(s1x())(s2x()), (s1xs(), s2xs()))
                case _ => None)

    def zipAll[B](s2: Stream[B]): Stream[ (Option[A], Option[B])] =
        Stream.unfold(this, s2)( (s1, s2) =>
            val tuple = (s1, s2)
            tuple match
                case (Cons(s1x,s1xs), Cons(s2x,s2xs)) => Some( (Some(s1x()),Some(s2x())), (s1xs(), s2xs()))
                case (Cons(s1x,s1xs), _) => Some( (Some(s1x()),None), (s1xs(), Empty))
                case (_, Cons(s2x,s2xs)) => Some( (None, Some(s2x())), (Empty, s2xs()))
                case (Empty, Empty) => None) 

    def startsWith[AA >:A](s2: Stream[AA]): Boolean =
        this.zipWithUnfold(s2)( a => b => (a,b)).forAll(_ == _)

    def tails: Stream[Stream[A]] =
        Stream.unfold(this)( xs =>
            xs match
                case Empty => None
                case Cons(x,xs) => Some(Stream.cons(x(),xs()), xs()))

    def hasSubsequence[A](s: Stream[A]): Boolean =
        this.tails.exists(_.startsWith(s))

    def scanRight[B](z: B)(f: (A, =>B) => B): Stream[B] =
        this.foldRight(Stream.cons(z,Empty))( (a,b) => 
            b match
                case Empty => Empty
                case Cons(h,t) => Stream.cons(f(a,h()), b))

    def tails1: Stream[Stream[A]] =
        this.scanRight(Empty: Stream[A])( (a,b) => Stream.cons(a, b))

    def tail2: Stream[Stream[A]] =

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h : () => A, t: ()  => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }
    
    def empty[A] : Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))

    def constant[A](a : A): Stream[A] =
        cons(a, Stream.constant(a))
    
    def from(n : Int): Stream[Int] =
        cons(n, from(n+1))
    
    def fibs : Stream[Int] =
        def go(a: Int, b: Int): Stream[Int] = {
            cons(a, go(b, a + b))
        }
        go(0,1)

    def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] =
        f(z) match
            case None => Empty
            case Some(a,s) => cons(a, unfold(s)(f))

    def fibs1 : Stream[Int] =
        unfold((0,1))((a,b) => Some(a, (b, a + b)))


    def from1(n : Int): Stream[Int] =
        unfold(n)(n => Some(n,n+1))

    def constant1[A](a : A): Stream[A] =
        unfold(a)(a => Some((a,a)))
}