ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "WordCounterInZIo"
  )

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "1.0.12",
  "dev.zio" %% "zio-streams" % "1.0.12"
)