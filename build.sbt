ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val `mars_rover_challenge` = (project in file("."))
  .settings(
    name := "mars_rover_challenge",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.8",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
      "org.codehaus.janino" % "janino" % "3.1.6",
      "ch.qos.logback" % "logback-classic" % "1.2.11" % Runtime,
      "org.scalatest" %% "scalatest" % "3.2.11" % Test
    )
  )
