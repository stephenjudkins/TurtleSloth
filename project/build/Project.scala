import sbt._

class ScalaProject(info: ProjectInfo) extends DefaultProject(info) {
  override def mainClass = Some("Main")
}