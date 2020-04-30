scalaVersion := "2.12.10" 
name := "schritt"
version := "1.0"
 

scalacOptions += "-Ypartial-unification" // 2.11.9+
scalacOptions ++= Seq("-deprecation", "-feature")



//libraryDependencies += "org.scala-lang"         % "scala-library"       % "2.12.10"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml"          % "2.0.0-M1"
//libraryDependencies += "org.scala-lang.modules" %% "scala-xml"          % "1.2.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalatest"          %% "scalatest"          % "3.0.8" % Test
libraryDependencies += "org.scalaz"             %% "scalaz-core"        % "7.3.0-M30"
//libraryDependencies += "org.scalaz"             %% "scalaz-core_2.12"   % "7.3.0-M30"
libraryDependencies += "org.scalaz"             %% "scalaz-effect"      % "7.3.0-M30"
libraryDependencies += "org.scalaz"             %% "scalaz-scalacheck-binding" % "7.3.0-M30"
libraryDependencies += "org.scalaz.stream"      %% "scalaz-stream" % "0.8.6"
libraryDependencies += "org.spire-math"         %% "spire"         % "0.13.0"
    
    
    


//libraryDependencies ++= Seq(
//    "org.tpolecat"    %% "doobie-core"     % doobieVersion
//  , "org.tpolecat"    %% "doobie-postgres" % doobieVersion
//  , "org.tpolecat"    %% "doobie-specs2"   % doobieVersion
//  , "org.tpolecat"    %% "doobie-hikari"   % doobieVersion  
//  , "io.monix"        %% "monix"           % "3.0.0"
//  , "org.scalatest"   %% "scalatest"       % "3.2.0-M1" % Test
//  , "com.typesafe"    % "config"           % "1.4.0"
//, "com.zaxxer"      % "HikariCP"         % "3.4.2"
//, "com.ibm.db2.jcc" %% "db2jcc4"         % "10.1"

//)

//libraryDependencies += "com.codahale.metrics" % "metrics-healthchecks" % metricsVersion
//libraryDependencies += "com.typesafe" % "config" % "1.4.0"
//libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.30"
//libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.30"
//libraryDependencies += "com.codahale.metrics" % "metrics-core" % metricsVersion
//libraryDependencies += "com.codahale.metrics" % "metrics-jvm" % metricsVersion


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

//lazy val root = (project in file(".")).
 // settings(
 //   name := "schritt",
 //   version := "1.0",
 //   scalaVersion := "2.11.7"
 // )