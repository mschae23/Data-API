val scala3Version = "3.0.0"

credentials +=
    Credentials(
        "GitHub Package Registry",
        "maven.pkg.github.com",
        sys.env("GITHUB_USERNAME"),
        sys.env("GITHUB_TOKEN")
    )

lazy val root = project
    .in(file("."))
    .settings(
        name := "data-api",
        organization := "de.martenschaefer",
        version := "2.0.0",
        homepage := Some(url("https://github.com/mschae23/Data-API")),

        scalaVersion := scala3Version,

        libraryDependencies ++= Seq(
            "org.typelevel" %% "cats-core" % "2.6.1",
            // "org.typelevel" %% "cats-effect" % "3.1.1",

            "org.typelevel" %% "shapeless3-deriving" % "3.0.1",

            "com.google.code.gson" % "gson" % "2.8.7",

            "org.scalactic" %% "scalactic" % "3.2.9",
            "org.scalatest" %% "scalatest" % "3.2.9" % "test"
        ),

        resolvers ++= Seq(
            "GitHub Package Registry (mschae23/Data-API)" at "https://maven.pkg.github.com/mschae23/Data-API",
            Resolver.sonatypeRepo("releases"),
            Resolver.sonatypeRepo("snapshots")
        ),

        publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/mschae23/Data-API"),
        scmInfo := Some(ScmInfo(url("https://github.com/mschae23/Data-API"), "scm:git@github.com:mschae23/Data-API.git")),

        publishMavenStyle := true,
        versionScheme := Some("semver-spec")
    )
