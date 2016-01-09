// import com.typesafe.sbt.SbtNativePackager._
// import NativePackagerKeys._
// packageArchetype.java_server

name := "ESJ" //Email Scene Judge
version := "0.2"


// scala compile options
//"-unchecked", "-deprecation", "-encoding", "utf8" and so on
scalaVersion := "2.10.5"
scalacOptions := Seq("-feature")
ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = false) }
lazy val root = (project in file(".")).enablePlugins(PlayScala)

// disable documentation generation
sources in (Compile, doc) := Seq.empty
publishArtifact in (Compile, packageDoc) := false

// ivy log level: Quiet, DownloadOnly, FULL
//ivyLoggingLevel := UpdateLogging.Quiet

// parallel execution in test
parallelExecution in Test := true
fork in Test := false

libraryDependencies ++= {
  val akkaVersion = "2.3.4"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "org.scalatest" %% "scalatest" % "2.2.0",

    "com.googlecode.xmemcached" % "xmemcached" % "2.0.0",
    "org.hbase" % "asynchbase" % "1.7.0",
    "org.fusesource.stomp" % "scomp" % "1.0.0"

    //  "org.apache.hadoop" % "hadoop-core" % "0.20.205.0",
    //  "org.apache.hbase" % "hbase" % "0.90.4"
    //  jdbc,
    //  ws,
    //  "com.typesafe.slick" % "slick_2.10" % "2.1.0",
    //  "org.apache.hadoop" % "hadoop-hdfs" % "2.4.1",
    //  "org.apache.hadoop" % "hadoop-common" % "2.4.1",
    //  "org.apache.hbase" % "hbase-client" % "1.1.1",
    //  "org.apache.hbase" % "hbase-common" % "1.1.1",
    //  "org.apache.hbase" % "hbase-server" % "1.1.1"
  )
}
doc in Compile <<= target.map(_ / "none")
