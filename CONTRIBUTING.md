# Contributing to CAADataBridge

Thank you for your interest in contributing to CAADataBridge! This project is maintained by the Community of Asset Analysts (CAA) and we welcome contributions from the community.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [How to Contribute](#how-to-contribute)
- [Development Setup](#development-setup)
- [Submitting Changes](#submitting-changes)
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
