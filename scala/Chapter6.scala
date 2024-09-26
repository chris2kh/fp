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

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs match
    case Nil => 
        for {
            machine <- State.get()
        } yield (machine.candies, machine.coins)
    
    case i :: is => for {
        _ <- applyRules(i)
        result <- simulateMachine(is)
    } yield result
    
def applyRules(input: Input): State[Machine, Unit] = for {
        machine <- State.get()
        _ <- input match
            case _    if (machine.candies == 0) => State.put(machine)
            case Coin if (!machine.locked)      => State.put(machine)
            case Turn if (machine.locked)       => State.put(machine)
            case Coin if (machine.candies > 0)  => State.modify((machine: Machine) => 
                                                            Machine(false, machine.candies, machine.coins)) 
            case Turn                           => State.modify( (machine: Machine) =>
                                                            Machine(true, machine.candies-1, machine.coins+1))
    } yield ()


def applyRules2(input: Input): State[Machine, (Int, Int)] = for {
        machine <- State.get()
        _ <- input match
            case _    if (machine.candies == 0) => State.put(machine)
            case Coin if (!machine.locked)      => State.put(machine)
            case Turn if (machine.locked)       => State.put(machine)
            case Coin if (machine.candies > 0)  => State.modify((machine: Machine) => 
                                                            Machine(false, machine.candies, machine.coins)) 
            case Turn                           => State.modify( (machine: Machine) =>
                                                            Machine(true, machine.candies-1, machine.coins+1))
        updatedMachine <- State.get() 
    } yield (updatedMachine.candies, updatedMachine.coins)

def simulateMachine2(inputs: List[Input]): State[Machine, List[(Int, Int)]] = 
    //sequence List[State[Machine, (Int, Int)]] -> State[Machine, List(Int, Int)]
    State.sequence(inputs.map(applyRules2))

def simulateMachine3(inputs: List[Input]): State[Machine, (Int, Int)] = 
    for {
    _ <- State.sequence(inputs.map(applyRules))
    machine <- State.get()
    } yield(machine.candies, machine.coins)
            
