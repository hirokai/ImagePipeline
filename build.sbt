name := "ImagePipeline"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"

libraryDependencies += "gov.nih.imagej" % "imagej" % "1.47"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"

libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.4.0-M2"


libraryDependencies ++= Seq(
  "org.netbeans.api" % "org-netbeans-modules-keyring" % "RELEASE721",
  "org.netbeans.modules" % "org-netbeans-modules-keyring-impl" % "RELEASE721"
)

resolvers += "netbeans" at "http://bits.netbeans.org/maven2/"