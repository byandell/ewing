# AI Agent Command Governance, AGENTS.md & Configuration Architecture

This document summarizes the AI agent command execution model, command authorization policies, and how project-level governance in `AGENTS.md` interacts with global system configurations in `~/.gemini/config/`.

---

## 1. Overview of AI Agent Command Execution

The AI assistant operates as a pair-programming partner capable of reading codebase files, modifying source code, running build commands in the shell, and executing browser verification tasks.

### Command Execution Categories

| Category | Commands / Tools | Purpose | Execution & Approval Model |
| :--- | :--- | :--- | :--- |
| **Local Verification** | `quarto render`, `Rscript`, `ls`, `grep`, `find`, `cat` | Building static site assets, compiling Quarto Shinylive demos, inspecting directories | Pre-authorized / executed automatically to verify code changes |
| **Version Inspection** | `git status`, `git diff`, `git log` | Inspecting modified lines, tracking branch state, checking diffs | Pre-authorized read-only operations |
| **File Editing** | `write_to_file`, `replace_file_content` | Modifying `R/`, `demos/`, `vignettes/`, and documentation files | Executed on target files within the active workspace |
| **Browser Testing** | `browser_subagent` | Navigating to rendered HTML pages, inspecting WebAssembly console logs | Executed autonomously for empirical verification |
| **Version Control Mutation** | `git add`, `git commit`, `git push` | Staging changes, creating Git commits, pushing to remote repositories | **Restricted by Governance** (Left to user execution) |

---

## 2. Token Efficiency & User Control over Version Management

During early development iterations, automated `git commit` and `git push` calls were included in the workflow. However, this introduced several drawbacks:

1. **Token Inefficiency**: Executing multi-step Git staging, committing, and pushing sequences after minor code edits consumed extra conversational turns and context window tokens.
2. **Commit Granularity**: Automatic commits produced high-frequency intermediate commits rather than clean, user-curated commit histories.
3. **Deployment Control**: Pushing to `master` automatically triggered GitHub Actions builds, which was undesirable during rapid local iteration.

### The New Workflow Model

- **Agent Responsibility**: Write clean code, refactor functions, update documentation, and verify builds locally (`quarto render`).
- **User Responsibility**: Review local changes via `git status`/`git diff`, stage files, write commit messages, and push to GitHub when ready.

---

## 3. Project-Level Governance (`AGENTS.md`)

The repository root contains **[AGENTS.md](../../AGENTS.md)**, which defines project-specific rules that all AI assistants must strictly obey when working on the **ewing** codebase.

### Current `AGENTS.md` Rules for `ewing`

```markdown
## Shinylive & Development Guidelines

- **No Automatic Git Commit/Push:** Prepare file edits and run local verifications (`quarto render`, etc.), but DO NOT execute `git commit` or `git push`. Leave all staging, committing, and pushing for the user to execute manually.
- **Standard Helper Pattern:** All Quarto Shinylive `.qmd` demos in `demos/` should use `source("include_ewing.R")` and `render_shinylive_app("appFunction()", height = ...)` to dynamically load package R source files cleanly without code duplication.
- **R Vector Subsetting Rule:** When filtering vectors in R, ALWAYS use `grepl("^\\s*#'", lines)` with `!grepl(...)` or `grep(..., invert = TRUE)`. NEVER use `!grep(...)` (which evaluates `!2` -> `FALSE` in R and wipes out the entire vector to `character(0)`).
- **Roxygen Comment Stripping:** Inlining R code into `{shinylive-r}` WebAssembly blocks requires stripping roxygen comments (`^#'`) to prevent Pandoc JSON string serialization errors.
- **Git Hygiene:** Local Quarto build folders (`demos/site_libs/`, `demos/_extensions/`, `demos/.quarto/`) must be listed in `.gitignore` and never committed.
- **GitHub Pages Deployment:** Always include `touch docs/.nojekyll` and `mkdir -p docs/demos` in `.github/workflows/pkgdown.yaml` before deploying to `gh-pages` so GitHub Pages serves WebAssembly static assets properly.
- **Navbar Structure:** Main `_pkgdown.yml` site structure puts `demos` before `articles` (Guides). `demos/_quarto.yml` `Home` tab points to `../index.html` (the pkgdown homepage).
```

---

## 4. Global Configuration (`~/.gemini/config/AGENTS.md`)

While `AGENTS.md` in the repository root governs the **ewing** project specifically, the global configuration directory at `~/.gemini/config/` defines rules that apply **across all projects** (R packages, Python apps, Quarto docs, technical writing) on the user's system.

A complete reference template is stored in **[global_agents_template.md](global_agents_template.md)**.

### System Protection Boundary & Installation

System security boundary rules prevent AI tools from mutating files inside `~/.gemini/config/` automatically. To install the global rules template across all repositories on your system, copy the template from the terminal:

```bash
mkdir -p ~/.gemini/config
cp inst/doc/global_agents_template.md ~/.gemini/config/AGENTS.md
```

---

## 5. Summary Hierarchy of Configuration & Control

```
System Security Policy (Hardcoded Protection Boundaries)
       │
       ├── Global Configuration (~/.gemini/config/AGENTS.md) ──► Applies across ALL user projects
       │
       └── Workspace Configuration (AGENTS.md) ───────────────► Applies specifically to ewing project
              │
              └── IDE Session Permissions ────────────────────► Read/Write, Quarto, Rscript, git status
```

By combining project-level `AGENTS.md` guidelines with local verification workflows, AI pair-programming remains fast, efficient, token-frugal, and aligned with user preferences.
