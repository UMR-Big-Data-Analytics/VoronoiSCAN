ThisBuild / scalaVersion := "2.13.17"
ThisBuild / organization := "com.umr"
ThisBuild / organizationName := "UMR"

lazy val akkaSecureRepoUrl: Option[String] =
  sys.props.get("AKKA_SECURE_REPO_URL").orElse(sys.env.get("AKKA_SECURE_REPO_URL"))

ThisBuild / resolvers ++= akkaSecureRepoUrl.toSeq.flatMap { base =>
  Seq(
    "akka-secure-mvn".at(base),
    Resolver.url("akka-secure-ivy", url(base))(Resolver.ivyStylePatterns)
  )
}

lazy val commonSettings = Seq(
  javacOptions ++= Seq("--enable-preview", "-source", "21", "-target", "21"),
  fork := true,
  javaOptions ++= Seq(
    "--enable-preview",
    "--add-modules=jdk.incubator.vector",
    "--enable-native-access=ALL-UNNAMED",
    "-Djava.library.path=lib"
  ),
  Test / javaOptions ++= Seq("--enable-preview"),
  resolvers ++= Seq(
    "SciJava Public".at("https://maven.scijava.org/content/repositories/public/")
  )
)

lazy val jacksonDatabind = "com.fasterxml.jackson.core" % "jackson-databind" % "2.18.5"

lazy val scalactic = "org.scalactic" %% "scalactic" % "3.2.19"

lazy val scalatest = "org.scalatest" %% "scalatest" % "3.2.19" % Test


lazy val openCsv = "com.opencsv" % "opencsv" % "5.12.0"

lazy val fastutil = "it.unimi.dsi" % "fastutil" % "8.5.18"

lazy val slf4jApi = "org.slf4j" % "slf4j-api" % "2.0.17"

lazy val logback = "ch.qos.logback" % "logback-classic" % "1.5.20"

lazy val akkaVersion = "2.10.11"

lazy val akkaActorTyped = "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion

lazy val akkaActorTestkitTyped = "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test

lazy val akkaStream = "com.typesafe.akka" %% "akka-stream" % akkaVersion

lazy val akkaStreamTestkit = "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test

lazy val akkaRemote = "com.typesafe.akka" %% "akka-remote" % akkaVersion

lazy val akkaClusterTyped = "com.typesafe.akka" %% "akka-cluster-typed" % akkaVersion

lazy val akkaClusterMetrics = "com.typesafe.akka" %% "akka-cluster-metrics" % akkaVersion

lazy val akkaSerializationJackson = "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion

lazy val jcommander = "org.jcommander" % "jcommander" % "3.0"

lazy val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6"

lazy val jmhVersion = "1.37"

lazy val jmhCore = "org.openjdk.jmh" % "jmh-core" % jmhVersion

lazy val jmhGenerator = "org.openjdk.jmh" % "jmh-generator-annprocess" % jmhVersion

lazy val scalapbRuntime = "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion

lazy val scalapbProtoBuf =
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"

lazy val parallelCollections = "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"

lazy val zlibJni = "com.github.luben" % "zstd-jni" % "1.5.7-6"

lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    name := "core",
    libraryDependencies ++= Seq(
      scalactic, scalatest, openCsv, fastutil, slf4jApi, logback,
      akkaSerializationJackson, jacksonDatabind, parallelCollections
    ),
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "main" / "generated",
    assembly / test := {},
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      case PathList("META-INF", xs@_*) => MergeStrategy.discard
      case PathList("reference.conf") => MergeStrategy.concat
      case "logback.xml" => MergeStrategy.first
      case _ => MergeStrategy.first
    },
  )

lazy val voronoiScan = (project in file("voronoiscan"))
  .enablePlugins(DockerPlugin, JavaAppPackaging, AssemblyPlugin)
  .settings(
    commonSettings,
    name := "VoronoiSCAN",
    libraryDependencies ++= Seq(
      scalactic, scalatest, akkaActorTyped, akkaActorTestkitTyped, akkaStream, akkaStreamTestkit, akkaRemote,
      akkaClusterTyped, akkaClusterMetrics, akkaSerializationJackson, jcommander, scalaLogging, slf4jApi, logback,
      scalapbRuntime, scalapbProtoBuf, zlibJni, fastutil
    ),
    Compile / PB.targets := Seq(
      scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
    ),
    Compile / mainClass := Some("App"),
    assembly / mainClass := Some("App"),
    assembly / assemblyJarName := "voronoiscan.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      case PathList("META-INF", xs@_*) => MergeStrategy.discard
      case PathList("reference.conf") => MergeStrategy.concat
      case "logback.xml" => MergeStrategy.first
      case _ => MergeStrategy.first
    },
    assembly / test := {}
  )
  .dependsOn(core % "test->test;compile->compile")

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "VoronoiSCAN"
  )
  .aggregate(core, voronoiScan)
