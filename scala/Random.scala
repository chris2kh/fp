object Random {
    def randomPair(rng: RNG): ((Int, Int), RNG) = {
        val (n1,rng1) = rng.nextInt
        val (n2, rng2) = rng1.nextInt
        return ((n1,n2),rng2)
    }

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