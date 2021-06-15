organization in ThisBuild := "io.circe"

crossScalaVersions in ThisBuild := Seq("2.12.12", "2.13.4")
scalaVersion in ThisBuild := crossScalaVersions.value.last

githubWorkflowJavaVersions in ThisBuild := Seq("adopt@1.8")
githubWorkflowPublishTargetBranches in ThisBuild := Nil
githubWorkflowJobSetup in ThisBuild := {
  githubWorkflowJobSetup.in(ThisBuild).value.toList.map {
    case step @ WorkflowStep.Use("actions", "checkout", "v2", _, _, _, _, _) =>
      step.copy(params = step.params.updated("submodules", "recursive"))
    case other => other
  }
}
githubWorkflowBuild in ThisBuild := Seq(
  WorkflowStep.Sbt(
    List(
      "clean",
      "coverage",
      "scalastyle",
      "scalafmtCheckAll",
      "scalafmtSbtCheck",
      "test",
      "coverageReport"
    ),
    id = None,
    name = Some("Test")
  ),
  WorkflowStep.Use(
    "codecov",
    "codecov-action",
    "v1"
  )
)

val catsVersion = "2.3.0"
val circeVersion = "0.14.1"
val scalaMetaVersion = "4.4.21"

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-jawn" % circeVersion % Test,
    "io.circe" %% "circe-generic" % circeVersion % Test,
    "io.circe" %% "circe-literal" % circeVersion % Test,
    "org.scalameta" %% "munit-scalacheck" % "0.7.20" % Test
  ),
  scalacOptions ++= Seq("-target:jvm-1.8", "-Ywarn-unused:imports") ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 13 => Seq("-Ymacro-annotations")
      case _                       => Seq("-Ypartial-unification")
    }
  },
  testFrameworks += new TestFramework("munit.Framework"),
  addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.11.2").cross(CrossVersion.full))
)

lazy val root =
  project.in(file(".")).settings(commonSettings).aggregate(schema, gen, validation).dependsOn(schema, gen, validation)

lazy val schema = project
  .settings(moduleName := "circe-schema")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-pointer" % circeVersion,
      "org.typelevel" %% "cats-core" % catsVersion
    )
  )

lazy val gen = project
  .settings(moduleName := "circe-schema-gen")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % scalaMetaVersion
    )
  )
  .dependsOn(schema)

lazy val validation =
  project
    .settings(
      moduleName := "circe-schema-validation",
      libraryDependencies ++= Seq("io.circe" %% "circe-parser" % circeVersion)
    )
    .settings(commonSettings)
    .dependsOn(schema)
