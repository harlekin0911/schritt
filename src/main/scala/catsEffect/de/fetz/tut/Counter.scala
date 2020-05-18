package catsEffect.de.fetz.tut

object Counter {
	import cats.effect.{IO, Sync}
	import cats.effect.concurrent.Ref
	import cats.implicits._
	import scala.concurrent.ExecutionContext

	// Needed for triggering evaluation in parallel
	implicit val ctx = IO.contextShift(ExecutionContext.global)
  import  scala.language.higherKinds 
  
	class Worker[F[_]](number: Int, ref: Ref[F, Int])(implicit F: Sync[F]) {

		private def putStrLn(value: String): F[Unit] = F.delay(println(value));

		// modify, setzt mit der ersten Variablen den State die zweite wird zurückgeben, 
		// beinhaltet den Wert beim nehmen
		def start: F[Unit] = for {
			c1 <- ref.get
			_  <- putStrLn(show"got#$number >> $c1")
			c2 <- ref.modify(x => (x+2 , "Hello " + x))
			_  <- putStrLn(show"set#$number >> $c2")
		} yield ()
	}

	val program: IO[Unit] = for {
		ref <- Ref.of[IO, Int](0)
		w1  = new Worker[IO](1, ref)
		w2  = new Worker[IO](3, ref)
		w3  = new Worker[IO](5, ref)
		_   <- List(
				w1.start,
				w2.start,
				w3.start
				).parSequence.void
	} yield ()
	
	val prog : IO[Unit] = Ref.of[IO, Int](0).flatMap( r => 
	  List( new Worker[IO](1, r).start,  new Worker[IO](2, r).start,  new Worker[IO](3, r).start).parSequence.void).map(_ => ())
	  
	def main( args : Array[String]) : Unit = 
	  //program.unsafeRunSync
	  	prog.unsafeRunSync

}