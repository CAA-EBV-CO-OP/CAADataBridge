# Contributing to CAADataBridge

Thank you for your interest in contributing to CAADataBridge! This project is maintained by the Community of Asset Analysts (CAA) and we welcome contributions from the community.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [How to Contribute](#how-to-contribute)
- [Development Setup](#development-setup)
- [Submitting Changes](#submitting-changes)
- [Continuous Integration (CI)](#continuous-integration-ci)
- [Style Guidelines](#style-guidelines)
- [Reporting Bugs](#reporting-bugs)
- [Requesting Features](#requesting-features)

## Code of Conduct

By participating in this project, you agree to abide by our [Code of Conduct](CODE_OF_CONDUCT.md). Please read it before contributing.

## How to Contribute

1. **Fork** the repository on GitHub.
2. **Clone** your fork locally: `git clone https://github.com/YOUR-USERNAME/CAADataBridge.git`
3. **Create a branch** for your changes: `git checkout -b feature/your-feature-name`
4. **Make your changes** and commit them (see [Submitting Changes](#submitting-changes)).
5. **Push** your branch to GitHub: `git push origin feature/your-feature-name`
6. Open a **Pull Request** against the `main` branch of this repository.

## Development Setup

### Prerequisites

- [R](https://www.r-project.org/) (version 4.0 or higher recommended)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended IDE)
- Required R packages: `shiny`, `readr`, `dplyr`, `stringdist` (see `global.R` for the full list)

### Running Locally

1. Open the project in RStudio.
2. Copy `.Renviron.example` to `.Renviron` and fill in any required values.
3. Run `shiny::runApp()` from the R console, or click **Run App** in RStudio.

## Submitting Changes

- Write clear, descriptive commit messages in the imperative mood (e.g., `Fix column matching for edge case`).
- Keep pull requests focused — one feature or fix per PR.
- Reference related issues in your PR description (e.g., `Closes #42`).
- Ensure your changes do not break existing functionality before submitting.
- Update `CHANGELOG.md` with a brief description of your change under the appropriate version heading.

## Continuous Integration (CI)

Every pull request and push to `main` automatically runs two CI checks via GitHub Actions. Both checks must pass before a PR can be merged.

### Lint R Code (`lint.yml`)

**What it does:** Runs [lintr](https://lintr.r-lib.org/) — a static analysis tool for R — across all `.R` files in the repository. lintr checks for code style issues, common mistakes, and potential bugs without executing the code.

**What it catches:**
- Style violations (e.g., spacing, indentation, line length) per the [tidyverse style guide](https://style.tidyverse.org/)
- Use of undefined variables or functions
- Unused imports
- Overly complex or potentially buggy expressions

**What to do if it fails:** Run lintr locally before pushing:

```r
install.packages("lintr")
lintr::lint_dir(".")
```

Review each flagged line and fix the issue. Common fixes include adding spaces around operators, shortening long lines, and removing unused `library()` calls. Once all issues are resolved, push your updated branch and the check will re-run automatically.

### Shiny App Smoke Test (`shinytest2.yml`)

**What it does:** Uses [shinytest2](https://rstudio.github.io/shinytest2/) to launch the CAADataBridge Shiny app in a headless Chromium browser and verify that it starts up successfully without errors. This is a "smoke test" — it checks that the app is fundamentally working, not that every feature behaves correctly.

**What it catches:**
- Missing or broken R package dependencies
- Startup errors in `global.R`, `ui.R`, or `server.R`
- Syntax errors that prevent the app from loading
- Any crash that occurs immediately on launch

**What to do if it fails:** Run the app locally first:

```r
shiny::runApp()
```

Check the R console for error messages on startup. Common causes of failure include a missing package dependency (add it to `global.R`) or a syntax error introduced in a recent change. Fix the issue locally, confirm the app launches cleanly, then push your updated branch.

### Branch Protection

The `main` branch is protected with the following rules:

- **Pull request required** — no direct pushes to `main`; all changes must go through a PR
- **1 approving review required** — at least one maintainer must approve your PR
- **Stale approvals dismissed** — if you push new commits after receiving an approval, the approval is cleared and must be re-given on the updated code
- **Conversation resolution required** — all review comments must be marked resolved before merging

### Submitting an Issue

When you open a new issue, you will be prompted to choose a template:

- **Bug Report** — use this to report something that is broken or behaving unexpectedly
- **Feature Request** — use this to suggest a new feature or improvement
- **Ask a Question** — links to [Discussions](https://github.com/CAA-EBV-CO-OP/CAADataBridge/discussions), which is the right place for general questions so they remain searchable for others

Blank issues are disabled — please use a template so maintainers have the context they need to help you quickly.

## Style Guidelines

- Follow the [tidyverse style guide](https://style.tidyverse.org/) for R code.
- Use 2-space indentation.
- Keep lines under 100 characters where practical.
- Add comments for non-obvious logic.
- Name variables and functions descriptively using `snake_case`.

## Reporting Bugs

Please use the [Bug Report issue template](https://github.com/CAA-EBV-CO-OP/CAADataBridge/issues/new?template=bug_report.md) and include:

- A clear description of the problem
- Steps to reproduce the issue
- Expected vs. actual behavior
- R version, OS, and package versions

## Requesting Features

Please use the [Feature Request issue template](https://github.com/CAA-EBV-CO-OP/CAADataBridge/issues/new?template=feature_request.md) and describe:

- The problem your feature would solve
- Your proposed solution
- Any alternatives you've considered

## Questions?

If you have questions that aren't covered here, feel free to open a [Discussion](https://github.com/CAA-EBV-CO-OP/CAADataBridge/discussions) in the repository.

Thank you for helping improve CAADataBridge!
