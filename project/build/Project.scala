import sbt._

class ScalaProject(info: ProjectInfo) extends DefaultProject(info) {
  val specs = "org.scala-tools.testing" % "specs" % "1.6.5" from "http://specs.googlecode.com/files/specs_2.8.0-1.6.5.jar"
}