scalaVersion := "2.12.10" 
name := "schritt"
version := "1.0"
 

scalacOptions += "-Ypartial-unification" // 2.11.9+
scalacOptions ++= Seq("-deprecation", "-feature")



//libraryDependencies += "org.scala-lang"        % "scala-library"       % "2.12.10"
//libraryDependencies += "org.scala-lang.modules" %% "scala-xml"                % "2.0.0-M1"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml"                % "1.0.5"
//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scalatest"          %% "scalatest"          % "3.0.8" % Test
//libraryDependencies += "org.scalaz"             %% "scalaz-core"        % "7.3.0-M30"
libraryDependencies += "org.scalaz"             %% "scalaz-core"        % "7.1.11"
//libraryDependencies += "org.scalaz"             %% "scalaz-effect"      % "7.3.0-M30"
libraryDependencies += "org.scalaz"             %% "scalaz-effect"      % "7.1.11"
//libraryDependencies += "org.scalaz"             %% "scalaz-scalacheck-binding" % "7.3.0-M30"
libraryDependencies += "org.scalaz"             %% "scalaz-scalacheck-binding" % "7.1.11"
libraryDependencies += "org.scalaz.stream"      %% "scalaz-stream" % "0.8.6"
libraryDependencies += "org.spire-math"         %% "spire"         % "0.13.0"
libraryDependencies += "org.typelevel"          %% "cats-effect"  % "2.1.3"
libraryDependencies += "org.typelevel"          %% "cats-parse" % "0.3.3"
libraryDependencies += "org.typelevel"          %% "jawn-ast" % "1.1.2"    
    
    


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





