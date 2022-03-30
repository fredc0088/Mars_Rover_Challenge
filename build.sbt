ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val `mars_rover_itv_challenge` = (project in file("."))
  .settings(
    name := "mars_rover_itv_challenge",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.7",
      "org.scalatest" %% "scalatest" % "3.2.11" % Test
    )
  )
