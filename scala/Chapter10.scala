trait Monoid[A] {
    def op(x: A, y: A): A
    def zero: A
}

val intAddition =  new Monoid[Int] {
    def op(x: Int, y:Int): Int = x + y
    def zero:Int = 0
}


val intMultiplication =  new Monoid[Int] {
    def op(x: Int, y:Int): Int = x * y
    def zero:Int = 1
}

val booleanOr =  new Monoid[Boolean] {
    def op(x: Boolean, y:Boolean): Boolean = x || y
    def zero:Boolean = false
}

val booleanAnd =  new Monoid[Boolean] {
    def op(x: Boolean, y:Boolean): Boolean = x && y
    def zero:Boolean = true
}

def optionMonoid[A] = new Monoid[Option[A]] {
    def op(x:Option[A], y:Option[A]): Option[A] = x orElse y
    def zero = None
}

def endoMonoid[A] = new Monoid[A => A] {
    def op(f: (A => A), g: (A => A)): (A => A) = x => f (g(x))
    def zero = x => x
}

def dual[A](m : Monoid[A]) = new Monoid[A] {
    def op(x: A, y: A): A =
        m.op(y,x)
    
    def zero = m.zero
}

def foldMap[A,B](xs: List[A], m: Monoid[B])(f: A => B): B = 
    xs.foldLeft(m.zero)((acc,x) => m.op(acc, f(x)))

def foldRight[A,B](z: B)(f : (A,B) => B)(xs: List[A]): B =
    foldMap(xs, endoMonoid)(f.curried)(z)

def foldLeft[A,B](z: B)(f : (B,A) => B)(xs: List[A]): B =
    //foldRight((i: B) => i)((x:A, k) => num => k(f(num,x)) )(xs)(z)
    foldMap(xs, dual(endoMonoid))(a => b => f(b,a))(z)

def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length == 0) then m.zero
    else if (v.length == 1) then f(v.head)
    else
        val (v1,v2) = v.splitAt(v.length/2)
        m.op(foldMapV(v1, m)(f), foldMapV(v2, m)(f))

def ordered(v: IndexedSeq[Int]): Boolean =
    val res = foldMapV(v, endoMonoid)((x: Int) => (tuple : (Int, Boolean)) => {
        val (y, state) = tuple
        if (state == false) then (y, state)
        else (Math.min(x,y), x <= y)
    })(Int.MaxValue, true)
    res._2

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(left: String, wordCount: Int, right: String) extends WC


val wcMonoid = new Monoid[WC] {
    def op (f1: WC, f2: WC): WC = (f1, f2) match
        case (Stub(x), Stub(y)) => Stub(x+y)
        case (Part(left, wc, right), Stub(x)) => Part(left, wc, right + x)
        case (Stub(x), Part(left, wc, right)) => Part(x + left, wc, right)
        case (Part(left1, wc1, right1), Part(left2, wc2, right2)) => 
            Part(left1, wc1 + wc2 + (if (right1 + right2).isEmpty() then 0 else 1), right2)

    def zero: WC = Stub("")

}

def count(xs: String): WC = {
    def wc(c:Char): WC = 
        if (c.isWhitespace) then Part("", 0, "")
        else Stub(c.toString)
    foldMapV(xs, wcMonoid)(wc)
}

trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B =
        foldMap(as)(f.curried)(endoMonoid)(z)

    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B =
        foldRight(as)((a: B) => a)((a,k) => n => k(f(n, a)) )(z)

    def foldMap[A,B](as: F[A])(f: A => B)(m: Monoid[B]): B =
        foldLeft(as)(m.zero)((b,a) => m.op(b, f(a)))

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
        foldLeft(as)(m.zero)(m.op)
    
    def toList[A](as: F[A]): List[A] =
        foldLeft(as)( (x:List[A]) => x)( (k, a) => as => k(a::as))(Nil: List[A])
} 

 object FoldableList extends Foldable[List] {
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match
        case Nil => z
        case h::t => foldLeft(t)(f(z,h))(f)
}

object FoldableIndexedSeq extends Foldable[IndexedSeq] {
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B  = 
        if (as.isEmpty) then z
        else foldLeft(as.tail)(f(z, as.head))(f)
}

object FoldableStream extends Foldable[Stream] {
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B  =
        as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object FoldableTree extends Foldable[Tree] {
    override def foldMap[A,B](as: Tree[A])(f: A => B)(m: Monoid[B]): B = as match
        case Leaf(x) => f(x)
        case Branch(l, r) => m.op(foldMap(l)(f)(m), foldMap(r)(f)(m))

    //override def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B = as match
    //    case Leaf(x) => f(z,x)
    //    case Branch(l,r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
}

object FoldableOption extends Foldable[Option] {
    override def foldLeft[A, B](ma: Option[A])(z: B)(f: (B, A) => B): B  = ma match
        case None => z
        case Some(x) => f(z,x)
}

def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = 
    new Monoid[(A,B)] {
        def zero: (A, B) = (a.zero, b.zero)
        def op(x: (A, B), y: (A, B)): (A, B) = (a.op(x._1, y._1), b.op(x._2, y._2))
    } 

def funcMonoid[A,B](b: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
        def zero: A => B = a => b.zero
        def op(x: A => B, y: A => B): A => B = a =>
               b.op(x(a), y(a))
    }


def mapMergeMonoid[K,V](v: Monoid[V]): Monoid[Map[K,V]] =
    new Monoid[Map[K,V]] {
        def zero: Map[K, V] = Map[K,V]()
        def op(x: Map[K, V], y: Map[K, V]): Map[K, V] = 
            (x.keySet ++ y.keySet).foldLeft(zero)( (acc, k) =>
                acc.updated(k, v.op(x.getOrElse(k, v.zero), y.getOrElse(k, v.zero)))     
            )
    }


def bag[A](as: IndexedSeq[A]): Map[A, Int] = 
    FoldableIndexedSeq.foldMap(as)(x => Map(x -> 1))(mapMergeMonoid(intAddition))