addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.2")

addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.1.1")

val sbtDevOopsVersion = "3.9.0"

addSbtPlugin("io.kevinlee" % "sbt-devoops-scala"     % sbtDevOopsVersion)
addSbtPlugin("io.kevinlee" % "sbt-devoops-sbt-extra" % sbtDevOopsVersion)
addSbtPlugin("io.kevinlee" % "sbt-devoops-github"    % sbtDevOopsVersion)

addSbtPlugin("io.kevinlee" % "sbt-devoops-starter" % sbtDevOopsVersion)
