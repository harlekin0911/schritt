package fpis

//object SomethingIII {
//
//    sealed trait Async[A] {
//        def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
//        def map[B]    (f: A => B):        Async[B] = flatMap(f andThen (Return(_)))
//    }
//    case class Return[A](a: A) extends Async[A]
//    case class Suspend[A](resume: Par[A]) extends Async[A]
//    case class FlatMap[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]
//    
//    // Note that the resume argument to Suspend is now a Par[A] rather than a () => A (or
//    // a Function0[A]). The implementation of run changes accordingly�it now returns a
//    // Par[A] rather than an A, and we rely on a separate tail-recursive step function to reassociate
//    // the FlatMap constructors:
//    
//    @annotation.tailrec
//    def step[A](async: Async[A]): Async[A] = async match {
//        case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
//        case FlatMap(Return(x), f) => step(f(x))
//        case _ => async
//    }
//
//    def run[A](async: Async[A]): Par[A] = step(async) match {
//        case Return(a)     => Par.unit(a)
//        case Suspend(r)    => Par.flatMap(r)(a => run(a))
//        case FlatMap(x, f) => x match {
//            case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
//            case _ => sys.error("Impossible; `step` eliminates these cases")
//        }
//    }
//}
object odel {}