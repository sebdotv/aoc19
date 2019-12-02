name := "aoc19"

version := "0.1"

scalaVersion := "2.12.10"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"
libraryDependencies += "org.typelevel" %% "mouse"       % "0.23"
libraryDependencies += "org.typelevel" %% "kittens"     % "2.0.0"
libraryDependencies += "co.fs2"        %% "fs2-core"    % "2.1.0"
libraryDependencies += "co.fs2"        %% "fs2-io"      % "2.1.0"
