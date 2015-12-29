name := "Flute"

scalaVersion := "2.11.7"

scalacOptions in ThisBuild ++= Seq("-feature", "-unchecked", "-deprecation", "-language:implicitConversions")

enablePlugins(ScalaJSPlugin)

scalaJSStage in Global := FastOptStage

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"