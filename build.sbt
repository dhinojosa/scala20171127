
name := "Scala Bootcamp"

version := "1.0_SNAPSHOT"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
   "org.scalactic" %% "scalactic" % "3.0.4",
   "org.scalatest" %% "scalatest" % "3.0.4"
)

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE18)

EclipseKeys.withSource := true

EclipseKeys.withJavadoc := true
