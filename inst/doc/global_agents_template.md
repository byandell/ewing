# Global AI Assistant Guidelines (`~/.gemini/config/AGENTS.md`)

This template provides a comprehensive set of universal guidelines for AI coding assistants operating across **R**, **Python**, **Quarto/Markdown documentation**, and **technical writing** repositories.

To install this file globally on your system, run:

```bash
mkdir -p ~/.gemini/config
cp inst/doc/global_agents_template.md ~/.gemini/config/AGENTS.md
```

---

## 1. Version Control & Command Execution Governance

- **No Automatic Git Commit/Push:** Prepare all file edits and run local build/test verifications (`quarto render`, `pytest`, `R CMD check`, `npm test`), but **NEVER** execute `git commit` or `git push`. Leave all staging, committing, and pushing for the user to execute manually.
- **Empirical Local Verification:** Never declare a task complete, bug fixed, or feature implemented without running concrete local verification commands to verify clean execution.
- **Preserve Codebase Conventions:** Maintain existing docstrings, variable naming styles, architectural patterns, and file organizational structures.

---

## 2. R Language Guidelines

- **Vector Subsetting Safety:** When filtering vectors in R, ALWAYS use `grepl("^\\s*#'", lines)` with `!grepl(...)` or `grep(..., invert = TRUE)`. NEVER use `!grep(...)` (which evaluates `!2` -> `FALSE` in R and wipes out the entire vector to `character(0)`).
- **Explicit Package Namespacing:** Use explicit package prefixes (`pkg::func()`) in exported functions and Shiny app modules to avoid namespace collisions.
- **Roxygen Comment Stripping in WASM:** Inlining R code into WebAssembly blocks (`{shinylive-r}`) requires stripping roxygen comments (`^#'`) to prevent Pandoc JSON string serialization errors.
- **Vectorized Logic:** Prefer vectorized operations (`ifelse()`, `lapply()`, `map()`) over explicit `for` loops when manipulating data frames or atomic vectors.

---

## 3. Python Language Guidelines

- **Modern Type Annotations:** Use modern Python type hints (`str | None`, `list[int]`, `dict[str, Any]`) and explicit return type declarations for functions.
- **Immutable Default Arguments:** Never use mutable default arguments (`def func(items=[])`). Use explicit `None` guards (`def func(items: list | None = None): items = items or []`).
- **Environment & Dependency Hygiene:** Respect active virtual environments (`.venv`, `conda`, `uv`). Avoid modifying system Python packages directly.
- **Explicit Exception Handling:** Avoid silent `except Exception: pass` blocks. Catch specific exceptions (`ValueError`, `FileNotFoundError`) and log or re-raise errors with clear context.

---

## 4. Quarto, Markdown & Technical Documentation

- **Single H1 Title Tag:** Ensure each markdown or Quarto document has exactly one `# Title` tag at the top, followed by structured `##` and `###` heading hierarchy.
- **Relative Path Hygiene:** Verify that file links use valid relative paths (`../index.html`, `vignettes/`) and valid markdown link syntax `[link text](file:///path/to/file)`.
- **Explicit Code Chunk Tagging:** Always tag code blocks with appropriate language identifiers (`{shinylive-r}`, `python`, `bash`, `r`, `json`, `yaml`).
- **Jekyll Security (`.nojekyll`):** When deploying Quarto or static web assets to GitHub Pages (`gh-pages`), ensure `touch docs/.nojekyll` is included to prevent Jekyll from hiding directories starting with `_`.

---

## 5. Technical Writing & Synthesis

- **Concise & Direct Communication:** Keep responses focused on actionable solutions. Provide high-level summaries highlighting key decisions or open questions.
- **Empirical Evidence Base:** Base technical diagnoses strictly on actual terminal command output, browser console logs, and empirical test traces.
- **No Dummy Placeholders:** Provide complete, drop-in replacement code blocks and complete tables rather than truncated snippets or `// TODO` placeholders.
