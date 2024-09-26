object Chapter3 {
    def tail[A](xs : List[A] ): List[A] = xs match {
        case _::xs => xs
        case _ => throw new Exception("empty list")
    }

    def setHead[A](x: A, xs: List[A]): List[A] = x::xs

    def drop[A](n: Int, xs: List[A]): List[A] = n match {
        case 0 => xs
        case n => drop((n-1), xs.tail)
    }

    def dropWhile[A]( xs: List[A], p: (A => Boolean)): List[A] = xs match {
        case Nil => Nil
        case xs if p(xs.head) => dropWhile(xs.tail, p)
        case _ => xs
    }

    def dropWhile1[A]( xs: List[A]) (p: (A => Boolean)): List[A] = xs match {
        case Nil => Nil
        case xs if p(xs.head) => dropWhile1(xs.tail)(p)
        case _ => xs
    }

    def init[A](xs: List[A]): List[A] = xs match {
        case Nil => Nil
        case x::Nil => Nil
        case x::xss  => x :: init(xss)
    }

    def foldRight[A,B](xs: List[A], z : B)(f : (A,B) => B) : B = xs match {
      case Nil => z
      case x::xss => f(x, foldRight(xss, z)(f))
    }
    
    def length[A](xs: List[A]): Int = foldRight(xs, 0)((_,acc) => acc + 1)


    def foldLeft[A,B](xs: List[A], z : B)(f : (B,A) => B) : B = xs match {
        case Nil => z
        case x::xss => foldLeft(xss, f(z, x))(f)
    }

    def foldLeftR[A,B](xs: List[A], acc : B)(f : (B,A) => B) : B = {
        foldRight(xs, (b:B) => b)((a,callback) => b => f(callback(b), a) )(acc)
    }

    def foldRightL[A,B](xs: List[A], acc : B)(f : (A, B) => B) : B = {
        foldLeft(xs, (b:B) => b)((callback, a) => b => f(a, callback(b)) )(acc)
    }

    def append[A](l1 : List[A], l2: List[A]): List[A] =
        foldRight(l1, l2)(_::_)

    def concat[A](xss:List[List[A]]):List[A] = {
        foldRight(xss,Nil:List[A])(append)
    }

    def addOne(xs:List[Int]):List[Int] = xs match {
        case Nil => Nil
        case x::xss => (x+1)::addOne(xss)
    } 

    def doublestoStrings(xs:List[Double]):List[String] = xs match {
        case Nil => Nil
        case x::xss => x.toString()::doublestoStrings(xss)
    }

    def map[A, B](xs:List[A])(f : A => B):List[B] = xs match {
        case Nil => Nil
        case x::xss => f(x)::map(xss)(f) 
    }

    def filter[A](xs:List[A])(f : A => Boolean): List[A] = xs match {
        case Nil => Nil
        case x::xss if f(x) => x::filter(xss)(f)
        case _::xss => filter(xss)(f)
    }

    def flatMap[A,B](xs:List[A])(f: A => List[B]):List[B] = xs match {
        case Nil => Nil
        case x::xss => f(x) ++ flatMap(xss)(f)
    }

    
    def filter2[A](xs:List[A])(f : A => Boolean): List[A] =
        flatMap(xs)(x => if f(x) then List(x) else List())
    
    def newListFrom(l1:List[Int], l2:List[Int]): List[Int] = {
        val tuple = (l1, l2)
        tuple match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (l1x::l1xs, l2x::l2xs) => (l1x + l2x)::newListFrom(l1xs, l2xs)
        }
    }

    def zipWith[A,B,C](l1:List[A], l2:List[B])(f: A => B => C):List[C] = {
        val tuple = (l1, l2)
        tuple match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (l1x::l1xs, l2x::l2xs) => f(l1x)(l2x)::zipWith(l1xs, l2xs)(f)
        }
    }

    def isPrefix[A](l1:List[A], l2:List[A]):Boolean = {
        val tuple = (l1, l2)
        tuple match {
            case (Nil, _) => true
            case (_, Nil) => false
            case (h1::t1, h2::t2) => (h1 == h2) && isPrefix(t1, t2) 
        }
    }

    def isSubsequence2[A](l1:List[A], l2:List[A]):Boolean =
        isPrefix(l1, l2) || isSubsequence2(l1, l2.tail)

    def size[A](tree : Tree[A]): Int = 
        tree match
            case Leaf(_) => 1
            case Branch(l, r) => size(l) + size(r)
        
    def maximum(tree : Tree[Int]): Int = 
        tree match
            case Leaf(x) => x
            case Branch(l, r) => maximum(l).max((maximum(r)))

    def depth[A](tree : Tree[A]): Int =
        tree match
            case Leaf(_) => 1
            case Branch(l, r) => 1 + depth(l).max(depth(r))

    def map[A, B](tree : Tree[A])(f : A => B):Tree[B] =
        tree match
            case Leaf(x) => Leaf(f(x))
            case Branch(l, r) => Branch(map(l)(f), map(r)(f))     

    def fold[A, B](tree : Tree[A])(f: A =>B)(g: (B,B) => B): B =
        tree match
            case Leaf(x) => f(x) 
            case Branch(l, r) =>  g(fold(l)(f)(g), fold(r)(f)(g))

    def size1[A](tree : Tree[A]): Int = 
        fold(tree)(_ => 1)(_+_)
        
    def maximum1(tree : Tree[Int]): Int = 
        fold(tree)(x => x)(_.max(_))

    def depth1[A](tree : Tree[A]): Int =
        fold(tree)(_ => 1)((x,y) => x + y + 1)

    def map1[A, B](tree : Tree[A])(f : A => B):Tree[B] =
        fold(tree) (x => Leaf(f(x)):Tree[B]) ((x,y) => Branch(x,y))
     
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]