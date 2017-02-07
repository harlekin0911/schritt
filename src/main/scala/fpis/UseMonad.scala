package fpis

import fpis.state._

class UseMonad {
  
    def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
        def unit[A](a: => A): State[S,A] = State(s => (a, s))
        def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] = st flatMap f
    }
    
    val F = stateMonad[Int]
//    def zipWithIndex[A](as: List[A]): List[(Int,A)] =
//        as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) => for {
//            xs <- acc
//            n  <- getState
//            _  <- setState(n + 1)
//       } yield (n, a) :: xs).run(0)._1.reverse
}