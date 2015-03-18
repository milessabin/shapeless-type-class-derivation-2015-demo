organization := "com.chuusai"

name := "demo-scaladayssf-2015"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.0-RC1"
)

initialCommands in console := """import shapeless._, scaladays._"""

scalacOptions := Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked")
