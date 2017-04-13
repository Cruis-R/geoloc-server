
organization := "deductions"
name := """geo-group-server"""
version := "1.0-SNAPSHOT"

scalaVersion :=  "2.11.8"

// Archery is a two-dimensional R-Tree written in Scala. The implementation is immutable: adding and removing points from the tree produces a new tree, leaving the old one untouched. Due to structural sharing this operation is quite efficient.
// The name "archery" is a corruption of the word "R-Tree".

resolvers += "bintray/meetup" at "http://dl.bintray.com/meetup/maven"

libraryDependencies += "com.meetup" %% "archery" % "0.4.0"
libraryDependencies += bananaDependency
libraryDependencies += jenaDependency

val jenaVersion =  "3.2.0"
val bananaDependency = "org.w3" %%  "banana-jena" % "0.8.4-SNAPSHOT"
val jenaDependency = "org.apache.jena" % "apache-jena-libs" % jenaVersion  exclude("log4j", "log4j") exclude("org.slf4j", "slf4j-api" ) exclude( "org.apache.logging.log4j" , "log4j-slf4j-impl" )

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % Test

lazy val server_play = (project in file(".")) . enablePlugins(PlayScala)

