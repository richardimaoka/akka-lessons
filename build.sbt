lazy val lesson1 = (project in file(".")).
  settings(
    name         := "lesson1",
    version      := "1.0",
    scalaVersion := "2.12.1",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.4.17",
      "com.typesafe.akka" %% "akka-agent" % "2.4.17",
      "com.typesafe.akka" %% "akka-camel" % "2.4.17",
      "com.typesafe.akka" %% "akka-cluster" % "2.4.17",
      "com.typesafe.akka" %% "akka-cluster-metrics" % "2.4.17",
      "com.typesafe.akka" %% "akka-cluster-sharding" % "2.4.17",
      "com.typesafe.akka" %% "akka-cluster-tools" % "2.4.17",
      "com.typesafe.akka" %% "akka-contrib" % "2.4.17",
      "com.typesafe.akka" %% "akka-multi-node-testkit" % "2.4.17",
      "com.typesafe.akka" %% "akka-osgi" % "2.4.17",
      "com.typesafe.akka" %% "akka-persistence" % "2.4.17",
      "com.typesafe.akka" %% "akka-persistence-tck" % "2.4.17",
      "com.typesafe.akka" %% "akka-remote" % "2.4.17",
      "com.typesafe.akka" %% "akka-slf4j" % "2.4.17",
      "com.typesafe.akka" %% "akka-stream" % "2.4.17",
      "com.typesafe.akka" %% "akka-stream-testkit" % "2.4.17",
      "com.typesafe.akka" %% "akka-testkit" % "2.4.17",
      "com.typesafe.akka" %% "akka-distributed-data-experimental" % "2.4.17",
      "com.typesafe.akka" %% "akka-typed-experimental" % "2.4.17",
      "com.typesafe.akka" %% "akka-persistence-query-experimental" % "2.4.17",
      "org.iq80.leveldb"            % "leveldb"          % "0.7",
      "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8",
      "org.scalactic" %% "scalactic" % "3.0.1",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test",
      "commons-io"                  % "commons-io"                   % "2.5"              % "test"
    )
  )
