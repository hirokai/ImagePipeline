name := "ImagePipeline"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq("com.assembla.scala-incubator" %% "graph-core" % "1.9.1",
  "com.assembla.scala-incubator" %% "graph-dot" % "1.10.0",
  "com.sksamuel.scrimage" %% "scrimage-core" % "1.4.2")


libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"

libraryDependencies += "gov.nih.imagej" % "imagej" % "1.47"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"