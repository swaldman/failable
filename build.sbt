import com.typesafe.sbt.pgp.PgpKeys._

val mainProjectName = "failable"

lazy val root = (project in file(".")).settings(
  commonSettings,
  publishResolveSettings,
  name := mainProjectName + "-root",
  publish := {},
  publishLocal := {},
  publishSigned := {}
).aggregate( core, logging ).dependsOn( logging )

lazy val core = (project in file("core")).settings(
  commonSettings,
  publishResolveSettings,
  name := mainProjectName
)

lazy val logging = (project in file("logging")).settings(
  commonSettings,
  publishResolveSettings,
  name := mainProjectName + "-logging",
  libraryDependencies += "com.mchange" %% "mlog-scala" % "0.3.13"
).dependsOn( core )

lazy val commonSettings = Seq(
  organization := "com.mchange",
  version := "0.0.5-SNAPSHOT",
  scalaVersion := "2.12.10",
  crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.10"),
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked" /*, "-Xlog-implicits" */)
)

lazy val publishResolveSettings = {
  val nexus = "https://oss.sonatype.org/"
  val nexusSnapshots = nexus + "content/repositories/snapshots";
  val nexusReleases = nexus + "service/local/staging/deploy/maven2";

  Seq(
    resolvers += ("releases" at nexusReleases),
    resolvers += ("snapshots" at nexusSnapshots),
    resolvers += ("Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"),
    resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"),
    publishTo := {
      val v = version.value
      if (v.trim.endsWith("SNAPSHOT")) {
        Some("snapshots" at nexusSnapshots )
      }
      else {
        Some("releases"  at nexusReleases )
      }
    },
    pomExtra := {
      <url>https://github.com/swaldman/{mainProjectName}</url>
      <licenses>
        <license>
          <name>GNU Lesser General Public License, Version 2.1</name>
          <url>http://www.gnu.org/licenses/lgpl-2.1.html</url>
          <distribution>repo</distribution>
        </license>
        <license>
          <name>Eclipse Public License, Version 1.0</name>
          <url>http://www.eclipse.org/org/documents/epl-v10.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:swaldman/{mainProjectName}.git</url>
        <connection>scm:git:git@github.com:swaldman/{mainProjectName}</connection>
      </scm>
      <developers>
        <developer>
          <id>swaldman</id>
          <name>Steve Waldman</name>
          <email>swaldman@mchange.com</email>
        </developer>
      </developers>
    }
  )
}


