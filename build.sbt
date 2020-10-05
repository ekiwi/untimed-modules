val basicSettings = Seq(
  name := "untimed-modules",
  organization := "edu.berkeley.cs",
  scalaVersion := "2.12.12",
  scalaSource in Compile := baseDirectory.value / "src",
  scalaSource in Test := baseDirectory.value / "test",
  resourceDirectory in Test := baseDirectory.value / "test" / "resources",
)

val versionSettings = Seq(
  version := "0.1-SNAPSHOT"
)

val chiselSettings = Seq(
  // for structural bundles
  scalacOptions := Seq("-deprecation", "-unchecked", "-Xsource:2.11"),
  libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.4.0-RC3",
)

val otherDependencySettings = Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.3.0-RC3" % "test",
)

lazy val main = (project in file("."))
  .settings(basicSettings)
  .settings(versionSettings)
  .settings(chiselSettings)
  .settings(otherDependencySettings)
  .settings(
    // execute test in serial for now to avoid race conditions on shared files like test.btor
    parallelExecution := false,
  )

