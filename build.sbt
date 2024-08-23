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
    `gin-macros`,
    `gin-specify`
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
      "io.circe" %% "circe-core" % "0.14.5",
      "io.circe" %% "circe-parser" % "0.14.5"
    )
  )

lazy val management = (project in file("./management"))
  .settings(
    name := "management",
    scalaVersion := "2.13.13",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.16",
      "dev.zio" %% "zio-streams" % "2.0.16"
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
      "com.chuusai" %% "shapeless" % "2.3.10",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "2.10.0"
    )
  )
  .dependsOn(gin)

lazy val gin = (project in file("./gin"))
  .settings(
    name := "gin",
    scalaVersion := "2.13.13",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.10",
      "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.10",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    )
//    scalacOptions ++= Seq(
//      "-Ymacro-debug-lite"
//    )
  )

lazy val `gin-macros` = (project in file("./gin-macros"))
  .settings(
    name := "gin",
    scalaVersion := "2.13.13",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.10",
      "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.10",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    )
  )
  .dependsOn(
    gin % "test->test;compile->compile"
  )

lazy val `gin-specify` = (project in file("./gin-specify"))
  .settings(
    name := "gin",
    scalaVersion := "2.13.13",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.10",
      "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.10",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "io.scalaland" %% "chimney" % "1.4.0",
      "com.softwaremill.quicklens" %% "quicklens" % "1.9.7"
    )
  )
  .dependsOn(
    `gin-macros` % "compile->compile",
    gin % "compile->compile"
  )

lazy val reflex = (project in file("./reflex"))
  .settings(
    name := "reflex",
    scalaVersion := "2.13.13",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    )
  )
