import List.*

trait RNG {
    def nextInt : (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0XFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }

    
}

type Rand[+A] = RNG => (A, RNG) 

object Random {
    def randomPair(rng: RNG): ((Int, Int), RNG) = {
        val (n1,rng1) = rng.nextInt
        val (n2, rng2) = rng1.nextInt
        return ((n1,n2),rng2)
    }


    def boolean(rng: RNG): (Boolean, RNG) = 
        map(nonNegativeInt)((i: Int) => if (i % 2 == 0) then true else false)(rng)

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (n, rng1) = rng.nextInt
        val n1 = n match
            case Int.MinValue => Int.MaxValue
            case n if n < 0 => -n
            case n => n
        (n1, rng1)
        
    }

    def double(rng: RNG): (Double, RNG) = {
        val (n, rng1) = nonNegativeInt(rng)
        val n1 = (if (n == Int.MaxValue) then (n-1) else n) / Int.MaxValue
        (n1.toDouble, rng1)
    }

    def between(a: Int, b: Int)(rng: RNG): (Int, RNG) = {
        val (n, rng1) = double(rng)
        val n1 = Math.floor(Math.floor(n*b) + a)
        (n1.toInt, rng1)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (tuple,rng1) = randomPair(rng)
        ((tuple._1, tuple._2.toDouble), rng1)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val (tuple, rng1) = intDouble(rng)
        ((tuple._2, tuple._1), rng1)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (n1, rng1) = double(rng)
        val (n2, rng2) = double(rng1)
        val (n3, rng3) = double(rng2)
        ((n1,n2,n3), rng3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def go(count:Int)(acc: (List[Int], RNG)): (List[Int], RNG) =
            count match
                case 0 => acc
                case n => 
                    val (xs, rng) = acc
                    val (x, rng1)  = rng.nextInt
                    go(n-1)((x::xs), rng1)
        go(count)(Nil, rng) 
    }   

    // Continuation Passing Style
    def ints1(count: Int)(rng: RNG): (List[Int], RNG) = {
        def go(count:Int)(k: (List[Int], RNG) => (List[Int], RNG)): (List[Int], RNG) => (List[Int], RNG) =
            count match
                case 0 => k
                case n => go(count-1)((xs, rng) =>
                    val (x, rng1) = rng.nextInt
                    k((x::xs), rng1))
                 
        go(count)((x,y) => (x,y))(Nil, rng) 
    }   

    def map[A,B](ra: Rand[A])(f: A => B): Rand[B] =
        rng =>
            val (a, rng1) = ra(rng)
            (f(a), rng1)

    def double2: Rand[Double] =
        Random.map(nonNegativeInt)(n => ((if (n == Int.MaxValue) then (n-1) else n) / Int.MaxValue).toDouble)

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
        rng =>
            val (a, rng1) = ra(rng)
            val (b, rng2) = rb(rng1)
            (f(a,b), rng2)

    def sequence[A](ras: List[Rand[A]]): Rand[List[A]] = ras match
        case Nil => rng => (Nil, rng)
        case h::t => map2(h, sequence(t))(_::_)

    def ints2(count: Int): Rand[List[Int]] = 
        sequence(List.fill(count)(nonNegativeInt))

    def flatMap[A,B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
        rng =>
            val (a, rng1) = ra(rng)
            f(a)(rng1)
    
    def unit[A](a :A): Rand[A] =
        rng => (a,rng)

    def nonNegativeLessThan(n:Int): Rand[Int] =
        flatMap(nonNegativeInt)(i => 
            val mod = i % n
            if (i + (n-1) - mod >= 0) then unit(i)      
            else nonNegativeLessThan(n))

    def map_flatMap[A,B](ra: Rand[A])(f: A => B): Rand[B] =
        flatMap(ra)( i => unit(f(i)))

    def map2_flatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
        flatMap(ra)(a =>
        flatMap(rb)(b =>
        unit(f(a,b))))
}    

case class State[S,+A](run: S => (A,S)) {
    def map[B](f: A => B): State[S,B] = State( s =>
        val (a, s1) = this.run(s)     
        (f(a), s1)
    )

    def flatMap[B](f: A => State[S,B]): State[S,B] = State( s =>
        val (a, s1) = this.run(s)     
        f(a).run(s1)
    )
}

object List1 {
    def foldRight[A,B](acc: B)(f: A => B => B)(xs: List[A]): B = xs match
        case Nil => acc
        case h :: t => f(h)(foldRight(acc)(f)(t))

    def foldLeft[A,B](acc: B)(f: B => A => B)(xs: List[A]): B = xs match
        case Nil => acc
        case h :: t => foldLeft(f(acc)(h))(f)(t)
}





object State {
    def unit[A,S](a: A): State[S,A] = State( s => (a, s))

    def map2[A,B,C,S](sa: State[S,A], sb: State[S,B])(f: (A,B) => C): State[S,C] =
       sa.flatMap( a =>
       sb.flatMap( b =>
          unit(f(a,b)) 
        ))      

    def sequence[A,S](sas: List[State[S,A]]): State[S,List[A]] = sas match
        case Nil => unit(Nil)
        case h :: t => h.flatMap(x => 
                       sequence(t).flatMap(xs =>
                        unit(x::xs)))

    def sequenceFoldRight[A,S](sas: List[State[S,A]]): State[S,List[A]] = 
        List1.foldRight(unit(Nil))((sa: State[S,A]) => (acc: State[S, List[A]]) => map2(sa, acc)(_::_))(sas)

    def sequenceFoldLeft[A,S](sas: List[State[S,A]]): State[S,List[A]] = 
        List1.foldLeft(unit(Nil))((acc: State[S, List[A]]) => (sa: State[S, A]) => map2(sa, acc)(_::_))(sas.reverse)

    def get[S](): State[S,S] = State(s => (s,s))
    def put[S](s: S): State[S,Unit] = State( _ => ((),s))
    def modify[S](f: S =>S) : State[S,Unit] = 
        for {
            s <- get()
            _ <- put(f(s))
        } yield ()
}











trait Prop {
    def check: Either[(Prop.FailedCase, Prop.SuccessCount), Prop.SuccessCount]
}

case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen(State( rng =>
            val (x, rng1) = this.sample.run(rng)
            f(x).sample.run(rng1)
        ))

    def flatMap1[B](f: A => Gen[B]): Gen[B] =
        Gen(this.sample.flatMap(x => f(x).sample))

    def listOfN(n: Int): Gen[List[A]] = n match
        case 0 => Prop.unit(Nil)
        case _ => this.flatMap(a => listOfN((n-1)).flatMap(as => Prop.unit(a::as)))

        
}

object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = 
        Gen(State(rng => Random.between(start, stopExclusive)(rng)))  

    def unit[A](a: => A): Gen[A] =
        Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State(Random.boolean))


    def map[A,B](a: Gen[A])(f: A => B): Gen[B] =
        Gen(State( rng =>
            val (x, rng1) = a.sample.run(rng)     
            (f(x), rng1)
        ))

    def mapUnwrap[A,B](a: Gen[A])(f: A => B): Gen[B] =
        Gen(a.sample.map(f))

    def uncurry[A, B, C](f: A => B => C): (A,B) => C =
        (a,b) => f(a)(b)

    def curry[A, B, C](f: (A,B) => C): A  => B => C =
        a => b => f(a,b)

    def map2Unwrap[A,B,C](a: Gen[A], b: Gen[B])(f: A => B => C): Gen[C] =
        Gen(State.map2(a.sample, b.sample)(uncurry(f)))

    def map2[A,B,C](a: Gen[A], b: Gen[B])(f: A => B => C): Gen[C] =
        Gen(State( rng =>
            val (partialFun, rng1) = map(a)(f).sample.run(rng)
            val (x, rng2) = b.sample.run(rng1)
            (partialFun(x), rng2)    
        ))

    def sequence[A](xs: List[Gen[A]]): Gen[List[A]] = 
        List1.foldRight(unit(Nil))( (x: Gen[A]) => (acc: Gen[List[A]]) => map2Unwrap(x,acc)(curry(_::_)))(xs)

    def sequence1[A](xs: List[Gen[A]]): Gen[List[A]] = 
         Gen(State.sequence(xs.map(_.sample)))


    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
        sequence1(List.fill(n)(g))

    def union[A](x: Gen[A], y: Gen[A]): Gen[A] = Gen(State(
        rng =>
            val (bool, rng1) = Random.boolean(rng)
            (if (bool) then x else y).sample.run(rng1)
    ))

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(State(
        rng => 
            val threshold = g1._2 / (g1._2 + g2._2)
            val x = g1._1
            val y = g2._1
            val (rand, rng1) = Random.double(rng)
            (if (rand <= threshold) then x else y).sample.run(rng1)
    ))
}

object Prop {
    type FailedCase = String
    type SuccessCount = Int
    

}