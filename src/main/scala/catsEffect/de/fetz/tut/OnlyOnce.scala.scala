package catsEffect.de.fetz.tut

object OnlyOnce {
  
  import cats.effect.IO
  import cats.effect.concurrent.Deferred
  import cats.implicits._
  import scala.concurrent.ExecutionContext

  // Needed for `start` or `Concurrent[IO]` and therefore `parSequence`
  implicit val cs = IO.contextShift(ExecutionContext.global)

  def start(d: Deferred[IO, Int]): IO[Unit] = {
    val attemptCompletion: Int => IO[Unit] = n => d.complete(n).attempt.void

    // d.get block's bis gefuellt, daher rennt immer das race zuerst, das füllt die das d
    val a : IO[List[Any]] = List(
      IO.race(attemptCompletion(1), attemptCompletion(2)),
      d.get.flatMap { n => IO(println(show"Result: $n")) }
    ).parSequence
    
    a.void
  }

  val program: IO[Unit] = for {
    d <- Deferred[IO, Int]
    _ <- start(d)
  } yield ()
  
  val prog : IO[Unit] = Deferred[IO,Int].flatMap( e => start(e))
  //val h : IO[Unit] = start(e)
      
  def main ( args: Array[String]) : Unit = prog.unsafeRunSync()
}