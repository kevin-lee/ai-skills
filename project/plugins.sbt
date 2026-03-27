addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.10")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

val sbtDevOopsVersion = "3.5.0"

addSbtPlugin("io.kevinlee" % "sbt-devoops-scala"     % sbtDevOopsVersion)
addSbtPlugin("io.kevinlee" % "sbt-devoops-sbt-extra" % sbtDevOopsVersion)
addSbtPlugin("io.kevinlee" % "sbt-devoops-github"    % sbtDevOopsVersion)

addSbtPlugin("io.kevinlee" % "sbt-devoops-starter" % sbtDevOopsVersion)
