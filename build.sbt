import scala.scalanative.build.*

ThisBuild / scalaVersion := props.ScalaVersion
ThisBuild / organization := props.Org

lazy val aiSkills = project
  .in(file("."))
  .enablePlugins(DevOopsGitHubReleasePlugin)
  .settings(
    name := props.ProjectName,
    /* GitHub Release { */
    devOopsPackagedArtifacts := List(
      s"modules/${props.ProjectName}-cli/target/scala-*/${props.ProjectName.replace("-", "")}-*",
    ),
    /* } GitHub Release */
  )
  .settings(noPublish)
  .aggregate(core, cli)

lazy val core = module("core")
  .enablePlugins(BuildInfoPlugin)
  .settings(
    /* Build Info { */
    buildInfoKeys := List[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoObject := "AiSkillsInfo",
    buildInfoPackage := "aiskills.info",
    buildInfoOptions += BuildInfoOption.ToJson,
    /* } Build Info */
  )
  .settings(
    libraryDependencies ++= Seq(
      libs.osLib.value,
      libs.scalaXml.value,
      libs.circeCore.value,
      libs.circeParser.value,
      libs.circeGeneric.value,
      libs.circeYamlScalayaml.value,
      libs.tests.hedgehogCore.value,
      libs.tests.hedgehogRunner.value,
      libs.tests.hedgehogSbt.value,
    ),
  )

lazy val cli = module("cli")
  .settings(
    libraryDependencies ++= Seq(
      libs.decline.value,
      libs.cue4s.value,
      libs.extrasScalaIo.value,
      libs.justSpinnerCore.value,
      libs.tests.hedgehogCore.value,
      libs.tests.hedgehogRunner.value,
      libs.tests.hedgehogSbt.value,
    ),
    nativeConfig ~= { c =>
      c.withBaseName(props.ProjectName.replace("-", ""))
        .withLTO(LTO.none)
        .withMode(Mode.releaseFast)
        .withGC(GC.commix)
    },
    Compile / mainClass := Some("aiskills.cli.Main"),
  )
  .dependsOn(core)

lazy val props = new {

  private val gitHubRepo = findRepoOrgAndName

  val GitHubUsername = gitHubRepo.fold("kevin-lee")(_.orgToString)
  val RepoName       = gitHubRepo.fold("ai-skills")(_.nameToString)
  val ProjectName    = RepoName

  val ScalaVersion = "3.8.3"

  val Org     = "io.kevinlee"
  val OrgName = "Kevin's Code"

  val OsLibVersion = "0.11.8"

  val CirceVersion = "0.14.15"

  val CirceYamlVersion = "0.16.1"

  val HedgehogVersion = "0.13.0"

  val DeclineVersion = "2.6.1"

  val Cue4sVersion = "0.0.10"

  val ExtrasVersion = "0.51.0"

  val ScalaXmlVersion = "2.4.0"

  val JustSpinnerVersion = "0.1.0"

  val licenses = List(License.MIT)
}

lazy val libs = new {
  lazy val osLib = Def.setting("com.lihaoyi" %%% "os-lib" % props.OsLibVersion)

  lazy val circeCore = Def.setting("io.circe" %%% "circe-core" % props.CirceVersion)

  lazy val circeParser = Def.setting("io.circe" %%% "circe-parser" % props.CirceVersion)

  lazy val circeGeneric = Def.setting("io.circe" %%% "circe-generic" % props.CirceVersion)

  lazy val circeYamlScalayaml = Def.setting("io.circe" %%% "circe-yaml-scalayaml" % props.CirceYamlVersion)

  lazy val decline = Def.setting("com.monovore" %%% "decline" % props.DeclineVersion)

  lazy val cue4s = Def.setting("tech.neander" %%% "cue4s" % props.Cue4sVersion)

  lazy val extrasScalaIo = Def.setting("io.kevinlee" %%% "extras-scala-io" % props.ExtrasVersion)

  lazy val scalaXml = Def.setting("org.scala-lang.modules" %%% "scala-xml" % props.ScalaXmlVersion)

  lazy val justSpinnerCore = Def.setting("io.kevinlee" %%% "just-spinner-core" % props.JustSpinnerVersion)

  lazy val tests = new {
    lazy val hedgehogCore = Def.setting("qa.hedgehog" %%% "hedgehog-core" % props.HedgehogVersion % Test)

    lazy val hedgehogRunner = Def.setting("qa.hedgehog" %%% "hedgehog-runner" % props.HedgehogVersion % Test)

    lazy val hedgehogSbt = Def.setting("qa.hedgehog" %%% "hedgehog-sbt" % props.HedgehogVersion % Test)
  }
}

lazy val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions ++= Seq("-Werror", "-deprecation", "-feature", "-unchecked"),
)


// format: off
def prefixedProjectName(name: String) = s"${props.ProjectName}${if (name.isEmpty) "" else s"-$name"}"
// format: on

def module(projectName: String): Project = {
  val prefixedName = prefixedProjectName(projectName)
  Project(projectName, file(s"modules/$prefixedName"))
    .settings(
      name := prefixedName,
    )
    .enablePlugins(ScalaNativePlugin)
    .settings(commonSettings)
    .settings(
      licenses := props.licenses,
    )
}
