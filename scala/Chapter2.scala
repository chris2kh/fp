
object Chapter2 {
    def fibonacci(n: Int): Int = {
        def iter(fib1 : Int, fib2: Int, round: Int): Int = {
            if (round == n) fib2
            else iter(fib2, fib1+fib2, round+1)
        }
        
        if (n == 0) 0
        else iter(0, 1, 1)
    }
    
    def isSorted[A] (xs: Array[A], ordered: (A,A) => Boolean): Boolean = {
        if (xs.length <=1) true 
        else if (ordered(xs(0), xs(1))) isSorted(xs.tail, ordered)
        else false
    }

    def curry[A,B,C] (f: (A,B) => C) : A => (B => C) =
        a => b => f(a,b)

    def uncurry[A,B,C] (f: A => B => C) : (A, B) => C =
        (a, b) => f(a)(b)


    def compose[A,B,C](f: B => C, g: A => B): A => C =
        a => f (g(a))


    def main(args: Array[String]) : Unit =
        println(fibonacci(0))
}