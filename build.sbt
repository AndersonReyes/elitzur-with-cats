import Dependencies._

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.github"
ThisBuild / organizationName := "andersonreyes"

lazy val root = (project in file("."))
  .settings(name := "elitzur-cats")

lazy val core = (project in file("core"))
  .settings(
    name := "elitzur-cats-core",
    libraryDependencies ++=
      Seq(
        scalaTest % Test,
        "com.chuusai" %% "shapeless" % "2.3.3",
        "org.typelevel" %% "cats-core" % "2.9.0",
        "com.spotify" %% "elitzur-core" % "0.6.14"
      )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
