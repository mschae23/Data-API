val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
      name := "codec",
      version := "0.1.0",

      scalaVersion := scala3Version,

      libraryDependencies ++= Seq(
          "org.typelevel" %% "cats-core" % "2.6.1",
          "org.typelevel" %% "cats-effect" % "3.1.1"
      )
  )
