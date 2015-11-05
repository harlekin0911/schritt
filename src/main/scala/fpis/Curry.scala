package functionalProgramming

class Curry {

    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
        (a: A) => ((b: B) => f(a, b))
        //(a: A) => f(a,_)
    }
  
    def uncurry[A,B,C](f: A => (B => C)): (A, B) => C = {
        (a:A, b:B) => f(a)(b) 
    }
    
    def compose[A,B,C](f: B => C, g: A => B): A => C = ( a: A) => f(g(a))
    
    
}

