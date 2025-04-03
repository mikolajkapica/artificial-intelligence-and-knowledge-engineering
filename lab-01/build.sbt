inThisBuild(
  List(
    scalaVersion := "3.3.5",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafmtConfig := new File("../.scalafmt.conf"),
  )
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "cats-effect" % "3.6.0",
  "co.fs2" %% "fs2-core" % "3.11.0",
  "co.fs2" %% "fs2-io" % "3.11.0",
  "org.gnieh" %% "fs2-data-csv" % "1.11.2",
  "org.gnieh" %% "fs2-data-csv-generic" % "1.11.2",
  "org.http4s" %% "http4s-ember-client" % "0.23.30",
  "io.circe" %% "circe-core" % "0.14.12",
  "io.circe" %% "circe-generic" % "0.14.12",
  "org.http4s" %% "http4s-circe" % "0.23.30",
  "com.monovore" %% "decline-effect" % "2.5.0",
  "org.scalameta" %% "munit" % "1.1.0" % Test,
  "org.typelevel" %% "munit-cats-effect" % "2.0-5e03bfc" % Test,
  "org.scalacheck" %% "scalacheck" % "1.18.1" % Test,
  "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
  "io.suzaku" %% "boopickle" % "1.5.0",
)
