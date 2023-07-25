lazy val root = (project in file("."))
  .aggregate(core, optics)

lazy val core = (project in file("./core"))
  .settings(
    name := "core",
    scalaVersion := "2.13.11",
    scalacOptions += "-Ymacro-annotations"
  )

lazy val optics = (project in file("./optics"))
  .settings(
    name := "optics",
    scalaVersion := "2.13.11",
    scalacOptions += "-Ymacro-annotations"
  )
  .dependsOn(core)
