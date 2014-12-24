// https://github.com/wookietreiber/scala-chart
val scalaChart = "com.github.wookietreiber" %% "scala-chart" % "latest.integration"

lazy val root = (project in file(".")).
  settings(
    name := "moneymaker",
    version := "1.0",
    scalaVersion := "2.11.2",
    libraryDependencies += scalaChart
  )
