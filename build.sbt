organization := "com.mchange"

name := "failable"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.6")

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked" /*, "-Xlog-implicits" */)

libraryDependencies ++= Seq(
)

// publish / resolve stuff

val nexus = "https://oss.sonatype.org/"
val nexusSnapshots = nexus + "content/repositories/snapshots";
val nexusReleases = nexus + "service/local/staging/deploy/maven2";

resolvers += ("releases" at nexusReleases)

resolvers += ("snapshots" at nexusSnapshots)

resolvers += ("Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/")

publishTo := {
  val v = version.value
  if (v.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexusSnapshots )
  }
  else {
    Some("releases"  at nexusReleases )
  }
}

pomExtra := {
    <url>https://github.com/swaldman/{name.value}</url>
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
      <url>git@github.com:swaldman/{name.value}.git</url>
      <connection>scm:git:git@github.com:swaldman/{name.value}</connection>
    </scm>
    <developers>
      <developer>
        <id>swaldman</id>
        <name>Steve Waldman</name>
        <email>swaldman@mchange.com</email>
      </developer>
    </developers>
}


