package catsEffect.de.fetz.tut

object Fairness {
  
  import java.util.concurrent.Executors
  import cats.effect.{ContextShift, Fiber, IO}
  import scala.concurrent.ExecutionContext

  val ecOne = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  val ecTwo = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  val csOne: ContextShift[IO] = IO.contextShift(ecOne)
  val csTwo: ContextShift[IO] = IO.contextShift(ecTwo)

  def infiniteIO(id: Int)(cs: ContextShift[IO]): IO[Fiber[IO, Unit]] = {
    def repeat: IO[Unit] = IO(println(id)).flatMap(_ => repeat)
    repeat.start(cs)
  }

  def infiniteIOShift(id: Int)(implicit cs: ContextShift[IO]): IO[Fiber[IO, Unit]] = {
    def repeat: IO[Unit] = IO(println(id)).flatMap(_ => IO.shift *> repeat)
    repeat.start
  }

  // No switch
  val prog : IO[Unit] = for {
    _ <- infiniteIO(1)(csOne)
    _ <- infiniteIO(11)(csOne)
  } yield ()
  
  // Switch only between Threadpools, these are on the CPU
  val prog2  : IO[Unit] = for {
    _ <- infiniteIO(1)(csOne)
    _ <- infiniteIO(11)(csOne)
    _ <- infiniteIO(2)(csTwo)
    _ <- infiniteIO(22)(csTwo)
  } yield ()
  
  
  // switch beetween Pools and Boundaries ( only locically)
  def prog3() : IO[Unit] = for {
    _ <- infiniteIOShift(1)(csOne)
    _ <- infiniteIOShift(11)(csOne)
    _ <- infiniteIOShift(2)(csTwo)
    _ <- infiniteIOShift(22)(csTwo)
  } yield ()      
  

  def main ( args : Array[String] ) : Unit = prog3.unsafeRunSync()
  
  
}