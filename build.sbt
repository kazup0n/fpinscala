name := "fpinscala"

version := "1.0"

scalaVersion := "2.11.11"


resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"


libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

logBuffered in Test := false
