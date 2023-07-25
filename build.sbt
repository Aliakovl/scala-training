ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

lazy val root = (project in file("."))
  .settings(
    name := "optics",
    scalacOptions += "-Ymacro-annotations",
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core" % "3.2.0" % Test,
      "dev.optics" %% "monocle-macro" % "3.2.0" % Test,
      "org.typelevel" %% "alleycats-core" % "2.9.0" % Test
    )
  )
