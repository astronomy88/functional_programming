/* 
Write a recursive function to get the nth Fibonacci 
number (http://mng.bz/C29s). The first two Fibonacci 
numbers are 0 and 1. The nth number is always the sum 
of the previous two—the sequence begins 
0, 1, 1, 2, 3, 5. Your definition should use a local 
tail-recursive function.
*/
object ChapterTwo {
    def fibonacci(n: Int) : Int = {
        @annotation.tailrec
        def go(n: Int, prev: Int, current: Int) : Int = 
            if (n <= 0) current
            else go(n-1, current, prev+current)
        go(n, 0, 1)
    }

    def findFirst[A](as: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int = 
            if (n >= as.length) -1
            else if (p(as(n))) n
            else loop(n + 1)
        loop(0)
    }

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        def loop(n: Int): Boolean = 
            if (n >= as.length) true
            else if (ordered(as(n), as(n+1))) false
            else loop(n + 1)
        loop(0)
    }

    def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
        (b: B) => f(a, b)

    def curry[A,B,C](f: (A, B) => C): A => (B => C) =
        (a: A) => (b: B) => f(a, b)
    
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
        (a: A, b: B) => f(a)(b)
    
    def compose[A,B,C](f: B => C, g: A => B): A => C =
        (a: A) => f(g(a))

    def main(args: Array[String]): Unit = {
        println(fibonacci(0))
    }
}