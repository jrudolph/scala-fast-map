import sbt._

class FastMapPlugin(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5-SNAPSHOT" % "test"
  
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
  override def packageAction = super.packageAction dependsOn(compile, test)
}
