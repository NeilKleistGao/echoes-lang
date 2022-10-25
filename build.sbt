ThisBuild / scalaVersion := "2.13.8"

lazy val root = project.in(file("."))
  .settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test,
    scalacOptions ++= Seq(
      "-deprecation"
    )
  )