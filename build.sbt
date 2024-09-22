ThisBuild / scalaVersion := "3.3.3"

lazy val `scala-training` = (project in file("."))
  .aggregate(
    core,
    optics,
    free,
    tffree,
    kernel,
    meta,
    awk,
    tf,
    management,
    `recursion-schemes`,
    `shapeless-guide`,
    reflex,
    gin,
  )

lazy val core = (project in file("./core"))
  .settings(
    name := "core",
    scalaVersion := "2.13.13",
    scalacOptions += "-Ymacro-annotations"
  )

lazy val free = (project in file("./free"))
  .settings(
    name := "free",
    scalaVersion := "2.13.13",
    scalacOptions += "-Ymacro-annotations"
  )
  .dependsOn(core)

lazy val optics = (project in file("./optics"))
  .settings(
    name := "optics",
    scalaVersion := "2.13.13",
    scalacOptions += "-Ymacro-annotations"
  )
  .dependsOn(core)

lazy val tffree = (project in file("./tffree"))
  .settings(
    name := "tffree",
    scalaVersion := "2.13.13",
    scalacOptions += "-Ymacro-annotations"
  )
  .dependsOn(core, free)

lazy val kernel = (project in file("./kernel"))
  .settings(
    name := "kernel",
    scalaVersion := "3.3.3"
  )

lazy val meta = (project in file("./meta"))
  .settings(
    name := "meta",
    scalaVersion := "3.3.3",
    scalacOptions += "-Xcheck-macros"
  )

lazy val awk = (project in file("./awk"))
  .settings(
    name := "awk",
    scalaVersion := "3.3.3",
    scalacOptions += "-Xcheck-macros"
  )

lazy val tf = (project in file("./tf"))
  .settings(
    name := "tf",
    scalaVersion := "3.3.3",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.9",
      "io.circe" %% "circe-parser" % "0.14.9"
    )
  )

lazy val management = (project in file("./management"))
  .settings(
    name := "management",
    scalaVersion := "2.13.13",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.6",
      "dev.zio" %% "zio-streams" % "2.1.6"
    )
  )

lazy val dtbrt = (project in file("./dtbrt"))
  .settings(
    name := "dtbrt",
    scalaVersion := "3.3.3"
  )

lazy val derive = (project in file("./derive"))
  .settings(
    name := "derive",
    scalaVersion := "3.3.3"
  )

lazy val `recursion-schemes` = (project in file("./recursion-schemes"))
  .settings(
    name := "recursion-schemes",
    scalaVersion := "3.3.3"
  )
  .dependsOn(kernel)

lazy val monad = (project in file("./monad"))
  .settings(
    name := "monad",
    scalaVersion := "3.3.3"
  )

lazy val `shapeless-guide` = (project in file("./shapeless-guide"))
  .settings(
    name := "shapeless-guide",
    scalaVersion := "2.13.13",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.12",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "2.12.0"
    )
  )
  .dependsOn(gin)

lazy val gin = (project in file("./gin"))
  .settings(
    name := "gin",
    scalaVersion := "2.13.13",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "2.12.0"
    ),
    scalacOptions ++= Seq(
      "--language:experimental.macros",
      "--language:implicitConversions"
    )
  )

lazy val reflex = (project in file("./reflex"))
  .settings(
    name := "reflex",
    scalaVersion := "2.13.13",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.12",
      "com.softwaremill.quicklens" %% "quicklens" % "1.9.8",
      "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.10",
      "com.softwaremill.macwire" %% "macros" % "2.5.9" % "provided",
      "com.softwaremill.sttp.tapir" %% "tapir-core" % "1.11.2",
      "io.scalaland" %% "chimney" % "1.4.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    )
  )
