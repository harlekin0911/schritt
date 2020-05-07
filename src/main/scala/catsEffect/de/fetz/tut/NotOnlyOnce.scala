package catsEffect.de.fetz.tut

//Use-case: Synchronized Mutable Variables
object SyncedVar {
    import cats.effect._
    import cats.effect.concurrent._
    import scala.concurrent.ExecutionContext

  // Needed for `start` or `Concurrent[IO]` and therefore `parSequence`
  implicit val cs = IO.contextShift(ExecutionContext.global)

  def sum(state: MVar[IO, Int], list: List[Int]): IO[Int] = list match {
      case Nil => state.take
      case x :: xs =>
        state.take.flatMap { current =>
          state.put(current + x).flatMap(_ => sum(state, xs))
        }
    }

    val a : IO[Int] = MVar.of[IO, Int](0).flatMap(sum(_, (0 until 100).toList))
    
    def main( args:Array[String]):Unit = println( "Sum is " + a.unsafeRunSync())
}

object NotOnlyOnce {
  
  import cats.effect.IO
  import cats.effect.concurrent.MVar
  import cats.implicits._
  import scala.concurrent.ExecutionContext
  
    // Needed for `start` or `Concurrent[IO]` and therefore `parSequence`
  implicit val cs = IO.contextShift(ExecutionContext.global)


  final class MLock(mvar: MVar[IO, Unit]) {
    
    // Use-case: Asynchronous Lock (Binary Semaphore, Mutex)
    // The take operation can act as “acquire” 
    // and put can act as the “release”. Let’s do it:
  
    def acquire: IO[Unit] = mvar.take

    def release: IO[Unit] = mvar.put(())

    def greenLight[A](fa: IO[A]): IO[A] = acquire.bracket(_ => fa)(_ => release)
  }

  object MLock {
    def apply() : IO[MLock] = MVar[IO].of(()).map(ref => new MLock(ref))
  }
}

object ProducerConsumer {
 // Use-case: Producer/Consumer Channel
 // An obvious use-case is to model a simple producer-consumer channel.

  // Say that you have a producer that needs to push events. 
  // But we also need some back-pressure, 
  // so we need to wait on the consumer to consume the last event 
  // before being able to generate a new event.

  import cats.effect._
  import cats.effect.concurrent._

  // Signaling option, because we need to detect completion
  type Channel[A] = MVar[IO, Option[A]]

  def producer(ch: Channel[Int], list: List[Int]): IO[Unit] = list match {
    case Nil          => ch.put(None) // we are done!
    case head :: tail => ch.put(Some(head)).flatMap(_ => producer(ch, tail)) // next please
  }

  def consumer(ch: Channel[Int], sum: Long): IO[Long] = ch.take.flatMap {
    case Some(x) => consumer(ch, sum + x) // next please
    case None    => IO.pure(sum) // we are done!
  }

  // ContextShift required for
  // 1) MVar.empty
  // 2) IO.start
  // for ContextShift documentation see https://typelevel.org/cats-effect/datatypes/contextshift.html
  import scala.concurrent.ExecutionContext
  implicit val cs = IO.contextShift(ExecutionContext.Implicits.global)

  var a : IO[Long] = for {
    channel <- MVar[IO].empty[Option[Int]]
    count = 100000
    producerTask = producer(channel, (0 until count).toList)
    consumerTask = consumer(channel, 0L)

    fp  <- producerTask.start
    fc  <- consumerTask.start
    _   <- fp.join
    sum <- fc.join
  } yield sum
  // Running this will work as expected. Our producer pushes values into our MVar and our consumer will consume all of those values.
  
  def main( args : Array[String]) : Unit = println ( "Sum out of thr Channel " + a.unsafeRunSync())
}