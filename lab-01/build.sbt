val toolkitV = "0.1.29"
val toolkit = "org.typelevel" %% "toolkit" % toolkitV
val toolkitTest = "org.typelevel" %% "toolkit-test" % toolkitV

inThisBuild(
  List(
    scalaVersion := "3.3.5",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafmtConfig := new File("../.scalafmt.conf"),
    scalafixConfig := Option(new File("../.scalafix.conf")),
  )
)

libraryDependencies += toolkit
libraryDependencies += (toolkitTest % Test)
