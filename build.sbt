lazy val lesson1 = (project in file(".")).
  settings(
    name         := "lessons",
    version      := "1.0",
    scalaVersion := "2.12.2",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.5.3",
      "com.typesafe.akka" %% "akka-agent" % "2.5.3",
      "com.typesafe.akka" %% "akka-camel" % "2.5.3",
      "com.typesafe.akka" %% "akka-cluster" % "2.5.3",
      "com.typesafe.akka" %% "akka-cluster-metrics" % "2.5.3",
      "com.typesafe.akka" %% "akka-cluster-sharding" % "2.5.3",
      "com.typesafe.akka" %% "akka-cluster-tools" % "2.5.3",
      "com.typesafe.akka" %% "akka-distributed-data" % "2.5.3",
      "com.typesafe.akka" %% "akka-multi-node-testkit" % "2.5.3",
      "com.typesafe.akka" %% "akka-osgi" % "2.5.3",
      "com.typesafe.akka" %% "akka-persistence" % "2.5.3",
      "com.typesafe.akka" %% "akka-persistence-query" % "2.5.3",
      "com.typesafe.akka" %% "akka-persistence-tck" % "2.5.3",
      "com.typesafe.akka" %% "akka-remote" % "2.5.3",
      "com.typesafe.akka" %% "akka-slf4j" % "2.5.3",
      "com.typesafe.akka" %% "akka-stream" % "2.5.3",
      "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.3",
      "com.typesafe.akka" %% "akka-testkit" % "2.5.3",
      "com.typesafe.akka" %% "akka-typed" % "2.5.3",
      "com.typesafe.akka" %% "akka-http" % "10.0.6",
      "com.typesafe.akka" %% "akka-contrib" % "2.5.3",
      "org.iq80.leveldb"             % "leveldb"          % "0.7",
      "org.fusesource.leveldbjni"    % "leveldbjni-all"   % "1.8",
      "org.scalactic" %% "scalactic" % "3.0.1",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test",
      "commons-io"    % "commons-io" % "2.5"   % "test"

    )
  )
