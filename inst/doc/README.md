# Ewing Package Documentation

This directory houses foundational user-facing documentation, dataset schemas, developer notes, and technical reference guides for the `ewing` package.

## Meta Documentation & AI Governance
- **[agents.md](agents.md)**: AI agent command governance, execution policies, and relationship between project `AGENTS.md` and global `~/.gemini/config/`.
- **[global_agents_template.md](global_agents_template.md)**: Universal global `AGENTS.md` template spanning R, Python, Quarto documentation, and technical writing.
- **[github_actions.md](github_actions.md)**: CI/CD GitHub Actions workflow setup, automated pkgdown site builds, and Shinylive WebAssembly deployment pipelines.
- **[demo_guide.md](demo_guide.md)**: Meta-documentation detailing the architecture, build pipeline, Jekyll 404 troubleshooting, and navbar navigation for the Shinylive Demos Gallery.
- **[devel_guide.md](devel_guide.md)**: Meta-documentation detailing the creation and migration of the package Developer Guide.
- **[tech_guide.md](tech_guide.md)**: Meta-documentation detailing prompts, architectural decisions, and results for creating the Technical Guide.

## Datasets & Modeling Architecture
- **[datasets.md](datasets.md)**: Outlines the Predator-Prey datasets, Excel workbook sheet schemas, and parameter structures injected into the simulation model.
- **[Developer Guide](../../vignettes/devel_guide/index.Rmd)**: Modular Shiny dashboard architecture, submodule index, and reactive workflows (rendered online at [articles/devel_guide/](https://byandell.github.io/ewing/articles/devel_guide/)).
- **[Technical Guide](../../vignettes/tech_guide/index.Rmd)**: Internal technical reference manual covering leftist-tree event queue mechanics, triangular coordinate geometry, thermal degree-day integration, and GIS watershed overlays (rendered online at [articles/tech_guide/](https://byandell.github.io/ewing/articles/tech_guide/)).
- **[Tutorial Vignette](../../vignettes/ewing.Rmd)**: End-to-end tutorial on simulation setup, execution, discretization, and envelope plotting.
