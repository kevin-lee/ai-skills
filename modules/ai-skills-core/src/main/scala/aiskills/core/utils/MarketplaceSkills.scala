package aiskills.core.utils

object MarketplaceSkills {

  /** Known skills from Anthropic's marketplace. Used to warn about potential conflicts. */
  val anthropicMarketplaceSkills: Set[String] = Set(
    // document-skills plugin
    "xlsx",
    "docx",
    "pptx",
    "pdf",
    // example-skills plugin
    "algorithmic-art",
    "artifacts-builder",
    "brand-guidelines",
    "canvas-design",
    "internal-comms",
    "mcp-builder",
    "skill-creator",
    "slack-gif-creator",
    "template-skill",
    "theme-factory",
    "webapp-testing",
  )
}
