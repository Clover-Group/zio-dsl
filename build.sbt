val CatsVersion      = "2.0.0-RC1"
val ParboiledVersion = "2.1.8"
val ScalaTestVersion = "3.0.8"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

organization := "Clover Group"
name := "zio-dsl"
version := "0.0.1"
scalaVersion := "2.12.8"
maxErrors := 3
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % CatsVersion,
  "org.parboiled" %% "parboiled" % ParboiledVersion,
  "org.scalactic" %% "scalactic" % ScalaTestVersion,
  "org.scalatest" %% "scalatest" % ScalaTestVersion % "test"
)

// Refine scalac params from tpolecat
scalacOptions --= Seq(
  "-Xfatal-warnings"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("chk", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("cvr", "; clean; coverage; test; coverageReport")
