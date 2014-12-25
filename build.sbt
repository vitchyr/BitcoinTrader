// https://github.com/wookietreiber/scala-chart
val scalaChart = "com.github.wookietreiber" %% "scala-chart" % "latest.integration"

// https://github.com/sameersingh/scalaplot -- Doesn't work for sbt
//val scalaplot = "org.sameersingh.scalaplot" %% "scalaplot" % "0.0.3"

//https://github.com/scalanlp/breeze
val breeze = "org.scalanlp" %% "breeze" % "0.10"
val breezeNative = "org.scalanlp" %% "breeze-natives" % "0.10"
//https://groups.google.com/forum/#!topic/scala-breeze/usONG50IY9w
val breezeViz = "org.scalanlp" %% "breeze-viz" % "0.8"

lazy val root = (project in file(".")).
  settings(
    name := "moneymaker",
    version := "1.0",
    scalaVersion := "2.11.2",
    libraryDependencies ++= Seq(
      scalaChart,
      breeze,
      breezeNative,
      breezeViz
    ),

    resolvers ++= Seq(
      // other resolvers here
      // if you want to use snapshot builds (currently 0.11-SNAPSHOT), use this.
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
      "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
    )
  )
