package catsEffect.de.fetz.tut

import cats.effect.IO

object IOtut {
  
  def main( args:Array[String]) : Unit = {
    standard()
    testAttemptThrow()
    testAttemptDontThrow()
  }
  
    def standard () = {
    val ioa = IO { println("hey!") }
    
    // IO[Unit]
    val program: IO[Unit] =
    for {
       _ <- ioa
       _ <- ioa
    } yield ()

    program.unsafeRunSync()
    ()
  }
  
  def testAttemptThrow () = {
    try {
        val boom : IO[Unit] = IO.raiseError(new Exception("boom"))
        boom.unsafeRunSync()
    } catch {
      case e: Throwable => println(e)
    }
  }
  def testAttemptDontThrow () = {
    try {
        val boom : IO[Unit] = IO.raiseError(new Exception("boom"))
        val a = boom.attempt
        println( "Exception not thrown: " + a.unsafeRunSync().left)
    } catch {
      case e: Throwable => println(e)
    }
  }
}