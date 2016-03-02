package fpis

// fpinscala 14.2

  
sealed trait ST[S,A] { self =>
    
    protected def run(s: S): (A,S)
    
    def map[B](f: A => B): ST[S,B] = new ST[S,B] {
        def run(s: S) = {
            val (a, s1) = self.run(s)
            (f(a), s1)
        }
    }
    
    def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
        def run(s: S) = {
            val (a, s1) = self.run(s)
            f(a).run(s1)
        }
    }
}

object ST {
    def apply[S,A](a: => A) = {
        lazy val memo = a
        new ST[S,A] { def run(s: S) = (memo, s)}
    }
    def runST[A](st: RunnableST[A]): A = st.apply[Unit].run(())._1
}

trait RunnableST[A] { def apply[S]: ST[S,A]}

sealed trait STRef[S,A] {
    protected var cell: A
    def read: ST[S,A] = ST(cell)
    def write(a: A): ST[S,Unit] = new ST[S,Unit] {
        def run(s: S) = {
            cell = a
            ((), s)
        }
    }
}

object STRef {
    def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] { var cell = a})
}

sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]
    def size: ST[S,Int] = ST(value.size)
    def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
        def run(s: S) = {
            value(i) = a
            ((), s)
        }
    }
    def read(i: Int): ST[S,A] = ST(value(i))
    def freeze: ST[S,List[A]] = ST(value.toList)
}

object STArray {
    def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] = ST(new STArray[S,A] {
        lazy val value = Array.fill(sz)(v)
    })
}


object testST {
    def testFlatMap = {
        val o  = STRef[Nothing,Int](1).flatMap( r1 => r1.read.flatMap( x => r1.write( x+1).map( _ => r1.read)))
        
        val oo = STRef[Nothing,Int](1).flatMap( r1 =>
                     STRef[Nothing,Int](2).flatMap( r2 =>  
                         r1.read.flatMap( x => 
                             r2.read.flatMap( y =>
                                r1.write( y+1).flatMap( x => 
                                    r2.write(y+1).flatMap( y => 
                                      r1.read.map( _ => r2.read))))))) 
    }
    
    def testRunFlatMap[S] = {
        
        STRef[S,Int](1).flatMap( (r1 : STRef[S, Int]) =>
            STRef[S,Int](2).flatMap( (r2  : STRef[S, Int]) =>  
                r1.read.flatMap( x => 
                    r2.read.flatMap( y =>
                        r1.write( y+1).flatMap( _ => 
                            r2.write(x+1).flatMap( _ => 
                                r1.read.flatMap( a => 
                                    r2.read.map( b => (a,b))))))))) 
    }

//    def testRunSTFlatMap[S] = {
//        
//        new RunnableST[(Int,Int)] {
//            def apply[S] = STRef[S,Int](1).flatMap( (r1 : STRef[S, Int]) =>
//                STRef[S,Int](2).flatMap( (r2  : STRef[S, Int]) =>  
//                    r1.read.flatMap( x => 
//                        r2.read.flatMap( y =>
//                            r1.write( y+1).flatMap( x => 
//                                r2.write(y+1).flatMap(  _ => (r1.read, r2.read))))))) 
//        }
//    }

    
    def main(arg: Array[String]) : Unit = {
        
        val dl = Array(3)
        
        val z = for {
            r1 <- STRef[Nothing,Int](1)
            x <- r1.read
            _ <- r1.write(x+1)
            a <- r1.read
        } yield a 
        //z.run

        val c = new RunnableST[(Int, Int)] {
                    def apply[S] = for {
                        r1 <- STRef(1)
                        r2 <- STRef(2)
                        x <- r1.read
                        y <- r2.read
                        _ <- r1.write(y+1)
                        _ <- r2.write(x+1)
                        a <- r1.read
                        b <- r2.read
                    } yield (a,b)
                }
        Console.println( ST.runST(c))
        
        Console.println(  ST.runST(new RunnableST[(Int, Int)] {def apply[S] = testRunFlatMap}))
    }
}