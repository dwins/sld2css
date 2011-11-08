import AssemblyKeys._

seq(assemblySettings: _*)

name := "sld2css"

version := "0.1"

organization := "org.opengeo"

scalaVersion := "2.9.1"

fork in run := true

resolvers ++= Seq(
  "opengeo" at "http://repo.opengeo.org/",
  "osgeo" at "http://download.osgeo.org/webdav/geotools/"
)

libraryDependencies += "org.geotools" % "gt-main" % "8-SNAPSHOT"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-swing" % _)
