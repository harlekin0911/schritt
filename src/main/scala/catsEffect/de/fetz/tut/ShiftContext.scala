package catsEffect.de.fetz.tut


import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Timer
import cats.data.NonEmptyList

//import scala.concurrent.ExecutionContext.Implicits.global

object ShiftContext {


	val cachedThreadPool : ExecutorService = Executors.newCachedThreadPool()
	val fixedThreadPool  : ExecutorService = Executors.newFixedThreadPool(3)
	
	val BlockingFileIO : ExecutionContext  = ExecutionContext.fromExecutor(fixedThreadPool)
	val Main = ExecutionContext.global
	
	implicit val  contextShift: ContextShift[IO] = contextShift( false)

	def contextShift( global: Boolean) : ContextShift[IO] = global match {
	  case true => IO.contextShift( ExecutionContext.global) 
	  case _    => IO.contextShift( BlockingFileIO)
	}
	
	def io( s:String): IO[Unit] = for {
		_     <- IO(println("Enter " + s))
		_     <- IO.shift(BlockingFileIO)
		name  <- IO(scala.io.StdIn.readLine())
		_     <- IO.shift( Main)
		_     <- IO(println(s"$s: $name!"))
		
	} yield ()
	

	def main( args:Array[String]) : Unit = 
		io("name: ").flatMap( 
		  _=> io("home: ")).flatMap( 
			  _ => program).flatMap( 
				  _=> paralellAsync()).flatMap(
					  _=> paralellSync()).flatMap(
						  _ => IO(fixedThreadPool.shutdown())).unsafeRunSync()
	          


  // make sure that you have an implicit ContextShift[IO] in scope. 
  // for parMapN
  import cats.implicits._ 
  val program : IO[Unit] = (io( "name"), io("city"), io( "street")).parMapN { (_, _, _) => () }
	
	def paralellAsync () :  IO[cats.data.NonEmptyList[Int]] = {
	  
	  
    import cats.syntax.parallel._
    import cats.effect.IO._
   

    // Needed for IO.start to do a logical thread fork
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    //implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

    val anIO = IO(1)

    NonEmptyList.of(anIO, anIO).parSequence

	}

	def paralellSync() : IO[NonEmptyList[Int]] = {
	  

    import cats.syntax.parallel._


    // Needed for IO.start to do a logical thread fork
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    //implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
    import cats.effect.IO._
    NonEmptyList.of(1, 2, 3).parTraverse { i => IO(i)}
	}
      
	
}