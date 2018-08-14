name := "pageviews"

version := "1.0"

scalaVersion := "2.12.1"

val circeVersion = "0.6.1"
val http4sVersion = "0.15.2a"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-parser" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.typelevel" %% "cats" % "0.8.1",
  "ch.qos.logback" % "logback-classic" % "1.1.3"
)
