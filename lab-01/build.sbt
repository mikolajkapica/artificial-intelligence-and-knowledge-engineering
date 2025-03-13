val toolkitVersion = "0.1.29"
val scalacheckVersion = "1.18.1"
val munitScalacheckVersion = "1.1.0"
val boopickle = "1.5.0"

inThisBuild(
  List(
    scalaVersion := "3.3.5",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafmtConfig := new File("../.scalafmt.conf"),
    scalafixConfig := Option(new File("../.scalafix.conf")),
  )
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "toolkit" % toolkitVersion,
  "org.typelevel" %% "toolkit-test" % toolkitVersion % Test,
  "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test,
  "org.scalameta" %% "munit-scalacheck" % munitScalacheckVersion % Test,
  "io.suzaku" %% "boopickle" % boopickle,
)
