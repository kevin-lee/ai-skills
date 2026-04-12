package aiskills.cli.commands

import aiskills.core.SkillSourceType
import aiskills.core.utils.{Dirs, SkillMetadata, Yaml}
import extras.scala.io.syntax.color.*

import scala.util.Try

object SkillDisplay {

  private val BaseDirLabel    = "Base directory:"
  private val SourceTypeLabel = "sourceType:"
  private val SourceLabel     = "source:"
  private val SubpathLabel    = "subpath:"
  private val NameLabel       = "name:"

  private val ColonCol = 2 + BaseDirLabel.length // 17

  def padLabel(label: String): String = {
    val pad = ColonCol - label.length
    (" " * pad) + label
  }

  def renderInfoBlock(skillPath: os.Path): Unit = {
    val baseDirDisplay = Dirs.displayPath(skillPath)
    println(s"${padLabel(BaseDirLabel).bold} ${baseDirDisplay.yellow.bold}")

    val metadataOpt = SkillMetadata.readSkillMetadata(skillPath)

    metadataOpt match {
      case None =>
        println(s"  ${"(.aiskills.json not found so no install metadata)".dim}")
      case Some(metadata) =>
        val sourceTypeStr = metadata.sourceType match {
          case SkillSourceType.Git => "git"
          case SkillSourceType.Local => "local"
        }
        println(s"${padLabel(SourceTypeLabel).bold} $sourceTypeStr")

        if metadata.source.nonEmpty then println(s"${padLabel(SourceLabel).bold} ${metadata.source}")
        else ()

        val subpathValue = metadata.subpath.filter(_.nonEmpty)
        val isRepoSource = metadata.sourceType match {
          case SkillSourceType.Git => true
          case SkillSourceType.Local => false
        }
        subpathValue match {
          case Some(v) =>
            println(s"${padLabel(SubpathLabel).bold} $v")
          case None if isRepoSource =>
            println(s"${padLabel(SubpathLabel).bold} <root>")
          case None => ()
        }
    }

    val skillMdPath = skillPath / "SKILL.md"
    val yamlName    =
      if os.exists(skillMdPath) then Try(os.read(skillMdPath))
        .toOption
        .fold("")(content => Yaml.extractYamlField(content, "name"))
      else ""
    println(s"${padLabel(NameLabel).bold} $yamlName")
  }
}
