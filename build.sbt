val scala3Version = "3.1.0"
val scala2Version = "2.13.6"
val globalVersion = "0.1.0-SNAPSHOT"

val deps = Seq(
  "org.scalactic" %% "scalactic" % "3.2.10",
  "org.scalatest" %% "scalatest" % "3.2.10" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
  "com.github.nscala-time" %% "nscala-time" % "2.30.0"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "aads2022",
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

val week01Name = "week01"
lazy val week01 = project
  .in(file(week01Name))
  .settings(
    name := week01Name,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

val week02Name = "week02"
lazy val week02 = project
  .in(file(week02Name))
  .settings(
    name := week02Name,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

val week03Name = "week03"
lazy val week03 = project
  .in(file(week03Name))
  .settings(
    name := week03Name,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

val week04Name = "week04"
lazy val week04 = project
  .enablePlugins(ScalaJSPlugin)
  .in(file(week04Name))
  .settings(
    name := week04Name,
    version := globalVersion,
    scalaVersion := "2.13.6",
    libraryDependencies ++= (
      deps ++
      Seq(
        "org.scala-js" %%% "scalajs-dom" % "1.1.0"
      )
    ),
    scalaJSUseMainModuleInitializer := true,
    mainClass := Some("Week04")
  )

val week05Name = "week05"
lazy val week05 = project
  .in(file(week05Name))
  .settings(
    name := week05Name,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

val week06Name = "week06"
lazy val week06 = project
  .in(file(week06Name))
  .settings(
    name := week06Name,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

val week07Name = "week07"
lazy val week07 = project
  .in(file(week07Name))
  .settings(
    name := week07Name,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

val week08Name = "week08"
lazy val week08 = project
  .in(file(week08Name))
  .settings(
    name := week08Name,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

val week09Name = "week09"
lazy val week09 = project
  .in(file(week09Name))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := week09Name,
    version := globalVersion,
    scalaVersion := scala2Version,
    libraryDependencies ++= deps ++ Seq(
      "org.apache.kafka" % "kafka-clients" % "3.1.0",
      "org.apache.kafka" %% "kafka-streams-scala" % "3.1.0",
      "org.apache.kafka" %% "kafka" % "3.1.0",
      "org.slf4j" % "slf4j-simple" % "1.7.36",
      "org.postgresql" % "postgresql" % "42.3.2"
    )
  )

val week10Name = "week10"
lazy val week10 = project
  .in(file(week10Name))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := week10Name,
    version := globalVersion,
    scalaVersion := scala2Version,
    libraryDependencies ++= deps ++ Seq(
      "org.apache.kafka" % "kafka-clients" % "3.1.0",
      "org.apache.kafka" %% "kafka-streams-scala" % "3.1.0",
      "org.apache.kafka" %% "kafka" % "3.1.0",
      "org.slf4j" % "slf4j-simple" % "1.7.36",
      "org.postgresql" % "postgresql" % "42.3.2"
    )
  )

val week11Name = "week11"
lazy val week11 = project
  .in(file(week11Name))
  .settings(
    name := week11Name,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

val week12Name = "week12"
lazy val week12 = project
  .in(file(week12Name))
  .settings(
    name := week12Name,
    version := globalVersion,
    scalaVersion := scala2Version,
    libraryDependencies ++= deps ++ Seq(
      "org.apache.kafka" % "kafka-clients" % "3.1.0",
      "org.apache.kafka" %% "kafka-streams-scala" % "3.1.0",
      "org.apache.kafka" % "kafka-streams-test-utils" % "3.1.0" % Test,
      "org.apache.kafka" %% "kafka" % "3.1.0",
      "org.slf4j" % "slf4j-simple" % "1.7.36"
    )
  )

val week13Name = "week13"
lazy val week13 = project
  .in(file(week13Name))
  .settings(
    name := week13Name,
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

