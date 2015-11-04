//import sbt._
//import Keys._

//object SchrittBuild extends Build {
//  val opts = Project.defaultSettings ++ Seq(
//    scalaVersion := "2.11.5",
//    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
//  )
//
//  lazy val root =
//    Project(id = "schritt",
//            base = file("."),
//            settings = opts ++ Seq(
//              onLoadMessage ~= (_ + nio2check())
//            )) 
//            
//  def nio2check(): String = {
//    val cls = "java.nio.channels.AsynchronousFileChannel"
//    try {Class.forName(cls); ""}
//    catch {case _: ClassNotFoundException =>
//      ("\nWARNING: JSR-203 \"NIO.2\" (" + cls + ") not found.\n" +
//       "You are probably running Java < 1.7; answers will not compile.\n" +
//       "You seem to be running " + System.getProperty("java.version") + ".\n" +
//       "Try `project exercises' before compile, or upgrading your JDK.")
//    }
//  }
//}

lazy val root = (project in file(".")).
  settings(
    name := "schritt",
    version := "1.0",
    scalaVersion := "2.11.7"
  )