import sbt._
import Keys._

object AntTaskHelpBuild extends Build {
  val buildSettings = Seq(
    version := "0.1",
    scalaVersion := "2.10.3",
    scalacOptions ++= Seq(
      "-Xlint", "-feature", "-target:jvm-1.6"))

  val resolverSettings = Seq(
    resolvers += Resolver.sonatypeRepo("public")
  )

  val libraryDependsSettings = Seq(
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.2.0",
      "org.apache.ant" % "ant" % "1.9.3",
      "org.apache.ant" % "ant-jsch" % "1.9.3"
    ))

  val simplegrep = 
    Project("anttaskview", file("."))
      .settings(buildSettings: _*)
      .settings(resolverSettings: _*)
      .settings(libraryDependsSettings: _*)
}

