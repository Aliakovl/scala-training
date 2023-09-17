lazy val `scala-training` = (project in file("."))
  .aggregate(core, optics)

lazy val core = (project in file("./core"))
  .settings(
    name := "core",
    scalaVersion := "2.13.11",
    scalacOptions += "-Ymacro-annotations"
  )

lazy val free = (project in file("./free"))
  .settings(
    name := "free",
    scalaVersion := "2.13.11",
    scalacOptions += "-Ymacro-annotations"
  )
  .dependsOn(core)

lazy val optics = (project in file("./optics"))
  .settings(
    name := "optics",
    scalaVersion := "2.13.11",
    scalacOptions += "-Ymacro-annotations"
  )
  .dependsOn(core)

lazy val tffree = (project in file("./tffree"))
  .settings(
    name := "tffree",
    scalaVersion := "2.13.11",
    scalacOptions += "-Ymacro-annotations"
  )
  .dependsOn(core, free)

lazy val kernel = (project in file("./kernel"))
  .settings(
    name := "kernel",
    scalaVersion := "3.3.1"
  )

lazy val meta = (project in file("./meta"))
  .settings(
    name := "meta",
    scalaVersion := "3.3.1",
    scalacOptions += "-Xcheck-macros"
  )

lazy val awk = (project in file("./awk"))
  .settings(
    name := "awk",
    scalaVersion := "3.3.1",
    scalacOptions += "-Xcheck-macros"
  )

lazy val tf = (project in file("./tf"))
  .settings(
    name := "tf",
    scalaVersion := "3.3.1",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.5",
      "io.circe" %% "circe-parser" % "0.14.5"
    )
  )
