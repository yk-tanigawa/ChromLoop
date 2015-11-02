name := "ChromLoop"

version := "1.0"

scalaVersion := "2.10.7"

val buildSettings = Defaults.defaultSettings ++ Seq(
  javaOptions += "-Xms2G -Xmx10G"
)

libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalanlp" % "breeze_2.10" % "0.7",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  "org.scalanlp" % "breeze-natives_2.10" % "0.7"
)

resolvers ++= Seq(
  // other resolvers here
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
