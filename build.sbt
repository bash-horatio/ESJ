name := "ESJ" //Email Scene Judge
version := "0.1"

scalaVersion := "2.10.5"
ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = false) }
lazy val root = (project in file(".")).enablePlugins(PlayScala)

libraryDependencies ++= Seq(
  "com.googlecode.xmemcached" % "xmemcached" % "2.0.0",
  "org.hbase" % "asynchbase" % "1.7.0",
  "org.fusesource.stomp" % "scomp" % "1.0.0"
//  "org.apache.hadoop" % "hadoop-core" % "0.20.205.0",
//  "org.apache.hbase" % "hbase" % "0.90.4"
//  "com.typesafe.slick" % "slick_2.10" % "2.1.0",
//  "org.apache.hadoop" % "hadoop-hdfs" % "2.4.1",
//  "org.apache.hadoop" % "hadoop-common" % "2.4.1",
//  "org.apache.hbase" % "hbase-client" % "1.1.1",
//  "org.apache.hbase" % "hbase-common" % "1.1.1",
//  "org.apache.hbase" % "hbase-server" % "1.1.1"
)
doc in Compile <<= target.map(_ / "none")
