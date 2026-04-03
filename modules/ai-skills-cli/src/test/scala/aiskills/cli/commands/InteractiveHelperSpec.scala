package aiskills.cli.commands

import aiskills.core.{Agent, Skill, SkillLocation}
import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object InteractiveHelperSpec extends Properties {

  private val dummyPath = os.pwd / "test"

  private def skill(name: String, location: SkillLocation, agent: Agent): Skill =
    Skill(
      name = name,
      description = s"$name description",
      location = location,
      agent = agent,
      path = dummyPath / name,
    )

  override def tests: List[Test] = List(
    // resolveLocations
    example("resolveLocations: only project skills returns Some(Project)", testResolveLocationsOnlyProject),
    example("resolveLocations: only global skills returns Some(Global)", testResolveLocationsOnlyGlobal),
    example("resolveLocations: both project and global returns None", testResolveLocationsBoth),
    example("resolveLocations: empty list returns None", testResolveLocationsEmpty),
    // resolveAgents
    example("resolveAgents: single agent returns Some(agent)", testResolveAgentsSingle),
    example("resolveAgents: multiple agents returns None", testResolveAgentsMultiple),
    example("resolveAgents: empty list returns None", testResolveAgentsEmpty),
    // agentLocationLabelsPlain
    example("agentLocationLabelsPlain: single project location", testPlainLabelsSingleProject),
    example("agentLocationLabelsPlain: single global location", testPlainLabelsSingleGlobal),
    example("agentLocationLabelsPlain: both locations sorted project-first", testPlainLabelsBothSorted),
    example("agentLocationLabelsPlain: both locations sorted even if input is reversed", testPlainLabelsBothReversed),
    // buildAgentLabel
    example("buildAgentLabel: single location", testBuildAgentLabelSingleLocation),
    example("buildAgentLabel: multiple locations", testBuildAgentLabelMultipleLocations),
    example("buildAgentLabel: agent with skills in one of two locations", testBuildAgentLabelPartialLocations),
    // reportLocationResolutionThen
    example("reportLocationResolutionThen: Some passes resolved to continuation", testReportLocationSome),
    example("reportLocationResolutionThen: None passes None to continuation", testReportLocationNone),
    // reportAgentResolutionThen
    example("reportAgentResolutionThen: Some passes resolved to continuation", testReportAgentSome),
    example("reportAgentResolutionThen: None passes None to continuation", testReportAgentNone),
  )

  // resolveLocations tests

  private def testResolveLocationsOnlyProject: Result = {
    val skills = List(
      skill("s1", SkillLocation.Project, Agent.Claude),
      skill("s2", SkillLocation.Project, Agent.Cursor),
    )
    InteractiveHelper.resolveLocations(skills) ==== Some(SkillLocation.Project)
  }

  private def testResolveLocationsOnlyGlobal: Result = {
    val skills = List(
      skill("s1", SkillLocation.Global, Agent.Claude),
    )
    InteractiveHelper.resolveLocations(skills) ==== Some(SkillLocation.Global)
  }

  private def testResolveLocationsBoth: Result = {
    val skills = List(
      skill("s1", SkillLocation.Project, Agent.Claude),
      skill("s2", SkillLocation.Global, Agent.Claude),
    )
    InteractiveHelper.resolveLocations(skills) ==== None
  }

  private def testResolveLocationsEmpty: Result =
    InteractiveHelper.resolveLocations(Nil) ==== None

  // resolveAgents tests

  private def testResolveAgentsSingle: Result = {
    val agentsWithCounts = List((Agent.Claude, 3))
    InteractiveHelper.resolveAgents(agentsWithCounts) ==== Some(Agent.Claude)
  }

  private def testResolveAgentsMultiple: Result = {
    val agentsWithCounts = List((Agent.Claude, 3), (Agent.Cursor, 2))
    InteractiveHelper.resolveAgents(agentsWithCounts) ==== None
  }

  private def testResolveAgentsEmpty: Result =
    InteractiveHelper.resolveAgents(Nil) ==== None

  // agentLocationLabelsPlain tests

  private def testPlainLabelsSingleProject: Result = {
    val labels = InteractiveHelper.agentLocationLabelsPlain(Agent.Claude, List(SkillLocation.Project))
    Result.all(
      List(
        labels.length ==== 1,
        labels.head ==== "(project, Claude): .claude/skills",
      )
    )
  }

  private def testPlainLabelsSingleGlobal: Result = {
    val labels = InteractiveHelper.agentLocationLabelsPlain(Agent.Claude, List(SkillLocation.Global))
    Result.all(
      List(
        labels.length ==== 1,
        labels.head ==== "(global, Claude): ~/.claude/skills",
      )
    )
  }

  private def testPlainLabelsBothSorted: Result = {
    val labels = InteractiveHelper.agentLocationLabelsPlain(
      Agent.Claude,
      List(SkillLocation.Project, SkillLocation.Global),
    )
    Result.all(
      List(
        labels.length ==== 2,
        labels(0) ==== "(project, Claude): .claude/skills",
        labels(1) ==== "(global, Claude): ~/.claude/skills",
      )
    )
  }

  private def testPlainLabelsBothReversed: Result = {
    val labels = InteractiveHelper.agentLocationLabelsPlain(
      Agent.Claude,
      List(SkillLocation.Global, SkillLocation.Project),
    )
    Result.all(
      List(
        labels.length ==== 2,
        labels(0) ==== "(project, Claude): .claude/skills",
        labels(1) ==== "(global, Claude): ~/.claude/skills",
      )
    )
  }

  // buildAgentLabel tests

  private def testBuildAgentLabelSingleLocation: Result = {
    val skills = List(
      skill("s1", SkillLocation.Project, Agent.Claude),
      skill("s2", SkillLocation.Project, Agent.Claude),
    )
    val label  = InteractiveHelper.buildAgentLabel(Agent.Claude, 2, skills)
    label ==== "Claude          (2 skill(s))  (project, Claude): .claude/skills"
  }

  private def testBuildAgentLabelMultipleLocations: Result = {
    val skills = List(
      skill("s1", SkillLocation.Project, Agent.Claude),
      skill("s2", SkillLocation.Global, Agent.Claude),
      skill("s3", SkillLocation.Project, Agent.Claude),
    )
    val label  = InteractiveHelper.buildAgentLabel(Agent.Claude, 3, skills)
    label ==== "Claude          (3 skill(s))  (project, Claude): .claude/skills, (global, Claude): ~/.claude/skills"
  }

  private def testBuildAgentLabelPartialLocations: Result = {
    val skills = List(
      skill("s1", SkillLocation.Global, Agent.Cursor),
      skill("s2", SkillLocation.Project, Agent.Claude),
    )
    val label  = InteractiveHelper.buildAgentLabel(Agent.Cursor, 1, skills)
    label ==== "Cursor          (1 skill(s))  (global, Cursor): ~/.cursor/skills"
  }

  // reportLocationResolutionThen tests

  private def testReportLocationSome: Result = {
    val resolved = SkillLocation.Project.some
    val result   = InteractiveHelper.reportLocationResolutionThen("Testing", resolved) {
      case Some(loc) => loc.toString
      case None => "none"
    }
    result ==== "Project"
  }

  private def testReportLocationNone: Result = {
    val result = InteractiveHelper.reportLocationResolutionThen("Testing", none[SkillLocation]) {
      case Some(loc) => loc.toString
      case None => "none"
    }
    result ==== "none"
  }

  // reportAgentResolutionThen tests

  private def testReportAgentSome: Result = {
    val skills   = List(skill("s1", SkillLocation.Project, Agent.Claude))
    val resolved = Agent.Claude.some
    val result   = InteractiveHelper.reportAgentResolutionThen(resolved, skills) {
      case Some(agent) => agent.toString
      case None => "none"
    }
    result ==== "Claude"
  }

  private def testReportAgentNone: Result = {
    val result = InteractiveHelper.reportAgentResolutionThen(none[Agent], Nil) {
      case Some(agent) => agent.toString
      case None => "none"
    }
    result ==== "none"
  }
}
