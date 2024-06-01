// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "%ORGANIZATION%"

lazy val commonSettings = Seq (
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % "3.5.4",
    "edu.berkeley.cs" %% "chiseltest" % "0.5.4" % "test"
  ),
  scalacOptions ++= Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    "-Ymacro-annotations"
  ),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.4" cross CrossVersion.full),
)

lazy val fpga_samples = (project in file("./external/fpga_samples/chisel")).
  settings(
    commonSettings,
    name := "fpga_samples"
  )

lazy val cpu_design = (project in file("./cpu_design")).
  settings(
    commonSettings,
    name := "cpu_design"
  ).
  dependsOn(fpga_samples)

lazy val root = (project in file("."))
  .aggregate(cpu_design, fpga_samples)
