ThisBuild / scalaVersion := "3.3.0"

lazy val `scala-training` = (project in file("."))
  .aggregate(core, optics, free, tffree, kernel, meta, awk, tf, management, `recursion-schemes`)

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

lazy val awk = (project in file("./awk"))
  .settings(
    name := "awk",
    scalaVersion := "3.3.0",
    scalacOptions += "-Xcheck-macros"
  )

lazy val tf = (project in file("./tf"))
  .settings(
    name := "tf",
    scalaVersion := "3.3.0",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.5",
      "io.circe" %% "circe-parser" % "0.14.5"
    )
  )

lazy val management = (project in file("./management"))
  .settings(
    name := "management",
    scalaVersion := "2.13.10",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.16",
      "dev.zio" %% "zio-streams" % "2.0.16"
    )
  )

lazy val dtbrt = (project in file("./dtbrt"))
  .settings(
    name := "dtbrt",
    scalaVersion := "3.3.0"
  )

lazy val derive = (project in file("./derive"))
  .settings(
    name := "derive",
    scalaVersion := "3.3.0"
  )

lazy val `recursion-schemes` = (project in file("./recursion-schemes"))
  .settings(
    name := "recursion-schemes",
    scalaVersion := "2.13.10",
  )
