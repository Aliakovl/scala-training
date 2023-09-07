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
    scalaVersion := "3.3.0"
  )

lazy val meta = (project in file("./meta"))
  .settings(
    name := "meta",
    scalaVersion := "3.3.0",
    scalacOptions += "-Xcheck-macros"
  )
