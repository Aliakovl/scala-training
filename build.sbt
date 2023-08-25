lazy val root = (project in file("."))
  .aggregate(core, optics)

lazy val core = (project in file("./core"))
  .settings(
    name := "core",
    scalaVersion := "2.13.11",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies += "org.typelevel" %% "cats-free" % "2.9.0"
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

lazy val expressionProblem = (project in file("./tffree"))
  .settings(
    name := "tffree",
    scalaVersion := "2.13.11",
    scalacOptions += "-Ymacro-annotations"
  )
  .dependsOn(core, free)
