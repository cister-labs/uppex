lazy val uppx = project.in(file("."))
  .settings(
    name := "uppx",
    version := "0.1",
    scalaVersion := "3.0.2", //"2.13.6",
    scalacOptions += "-new-syntax",
    assembly / mainClass := Some("uppx.Main"),
    assembly / assemblyJarName := "uppx.jar",
    assembly / assemblyMergeStrategy := {
//      case "poi" => MergeStrategy.discard
//      case PathList(ps @ _*) if ps exists (_.startsWith("poi-ooxml-lite")) => MergeStrategy.discard
//      case PathList(ps @ _*) if ps.last endsWith "module-info.class" => MergeStrategy.discard
//      case PathList(ps @ _*) if ps.last endsWith "poi-5.0.0.jar" => MergeStrategy.discard
//      case PathList("org","apache", _*) => MergeStrategy.discard
      case PathList("META-INF","versions","9","module-info.class") => MergeStrategy.first
//      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.defaultMergeStrategy(x) //last // first
//      MergeStrategy.defaultMergeStrategy
    },
    libraryDependencies ++= Seq(
//      "org.apache.poi" % "poi" % "5.0.0",
      "org.apache.poi" % "poi-ooxml" % "5.0.0", // exclude("org.apache.poi","poi"),
      "org.scala-lang.modules" %% "scala-parser-combinators" %  "2.0.0"
    ),
    excludeDependencies ++= Seq(
      ExclusionRule("org.apache.xmlgraphics","batik-all"),
      ExclusionRule("commons-logging", "commons-logging"),
      ExclusionRule("org.codehaus.woodstox","stax2-api"),
      ExclusionRule("org.bouncycastle","bcpkix-jdk15on"),
      ExclusionRule("org.bouncycastle","bcprov-jdk15on")
//      ExclusionRule("org.apache.xmlbeans","xmlbeans")
    )
  )



