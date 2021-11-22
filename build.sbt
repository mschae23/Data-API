val scala3Version = "3.0.1"

lazy val root = project
    .in(file("."))
    .settings(
        name := "data-api",
        organization := "de.martenschaefer",
        version := "5.5.2",
        homepage := Some(url("https://github.com/mschae23/data-api")),

        scalaVersion := scala3Version,

        libraryDependencies ++= Seq(
            "org.typelevel" %% "cats-core" % "2.6.1",
            "org.typelevel" %% "cats-effect" % "3.2.8",

            "org.typelevel" %% "shapeless3-deriving" % "3.0.3",

            "com.google.code.gson" % "gson" % "2.8.9",

            "org.scalactic" %% "scalactic" % "3.2.9",
            "org.scalatest" %% "scalatest" % "3.2.9" % "test"
        ),

        resolvers ++= Seq(
            "GitHub Package Registry (mschae23/data-api)" at "https://maven.pkg.github.com/mschae23/data-api",
            Resolver.sonatypeRepo("releases"),
            Resolver.sonatypeRepo("snapshots")
        ),

        publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/mschae23/data-api"),
        scmInfo := Some(ScmInfo(url("https://github.com/mschae23/data-api"), "scm:git@github.com:mschae23/data-api.git")),

        publishMavenStyle := true,
        versionScheme := Some("semver-spec")
    )

credentials += Credentials(Path.userHome / ".github" / ".credentials")
