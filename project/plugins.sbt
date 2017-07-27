addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.9.0")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
