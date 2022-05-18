ThisBuild / organization := "io.circe"

ThisBuild / crossScalaVersions := Seq("2.12.15", "2.13.7")
ThisBuild / scalaVersion := crossScalaVersions.value.last

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")
ThisBuild / githubWorkflowPublishTargetBranches := Nil
ThisBuild / githubWorkflowJobSetup := {
  (ThisBuild / githubWorkflowJobSetup).value.toList.map {
    case step @ WorkflowStep.Use(UseRef.Public("actions", "checkout", "v2"), _, _, _, _, _) =>
      step.copy(params = step.params.updated("submodules", "recursive"))
    case other => other
  }
}
ThisBuild / githubWorkflowBuild := Seq(
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
    UseRef.Public(
      "codecov",
      "codecov-action",
      "v1"
    )
  )
)

val catsVersion = "2.7.0"
val circeVersion = "0.14.1"
val scalaMetaVersion = "4.5.7"

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-jawn" % circeVersion % Test,
    "io.circe" %% "circe-generic" % circeVersion % Test,
    "io.circe" %% "circe-literal" % circeVersion % Test,
    "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
  ),
  scalacOptions ++= Seq("-target:jvm-1.8", "-Ywarn-unused:imports") ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 13 => Seq("-Ymacro-annotations")
      case _                       => Seq("-Ypartial-unification")
    }
  },
  testFrameworks += new TestFramework("munit.Framework"),
  addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.2").cross(CrossVersion.full))
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
