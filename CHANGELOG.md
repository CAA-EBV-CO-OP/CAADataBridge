# Changelog

All notable changes to the CAA Column Mapper will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Planned
- Batch processing for multiple CSV files
- Profile marketplace/sharing platform
- Advanced analytics visualizations
- Machine learning-enhanced suggestions

---

## [2.4.1] - 2026-03-02

### Added

- **GitHub Actions: Lint R Code** (`.github/workflows/lint.yml`): Runs [lintr](https://lintr.r-lib.org/) on all `.R` files on every push and pull request to `main`. Fails the check if any style violations, undefined variables, or other issues are found. Enforces the tidyverse style guide automatically on all contributions.
- **GitHub Actions: Shiny App Smoke Test** (`.github/workflows/shinytest2.yml`): Launches the full app in a headless Chromium browser using [shinytest2](https://rstudio.github.io/shinytest2/) on every push and pull request. Catches startup errors, missing dependencies, and crashes before they reach `main`.
- **Smoke Test File** (`tests/testthat/test-app-launch.R`): The `testthat`/`shinytest2` test that the CI workflow runs. Tests that the app initializes and responds without errors.
- **Issue Template Chooser** (`.github/ISSUE_TEMPLATE/config.yml`): Configures the GitHub issue creation UI to require a template (blank issues disabled), and adds contact links to Discussions for questions and to the CAA community site.

### Changed

- **Branch Protection on `main`**: Enabled "Dismiss stale pull request approvals when new commits are pushed." PR approvals are now cleared when new commits are added, ensuring reviews always reflect the final state of the code.
- **CONTRIBUTING.md**: Added a full [Continuous Integration (CI)](#continuous-integration-ci) section explaining both workflows (lintr and shinytest2), what each check does, what to do when one fails, branch protection rules, and how the issue template system works.

---

## [2.4.0] - 2026-02-25

### Added
- **Synonym Editing in Standard Mode**: Pencil icon + modal to edit field synonyms directly from the mapping UI
- **Decimal Baths Split (BA/PB)**: Decimal bath values (e.g., 2.1) automatically split into full baths (BA) and partial baths (PB)
  - BA mode toggle for switching between decimal and integer bath handling
  - Works in both Standard/RESO mode and Destination Schema mode
  - Auto-accept on pre-loaded profiles that previously accepted the conversion
- **Data Enrichment Features** *(optional, not yet enabled in distribution)*:
  - Status normalization for standardizing listing status values
  - SubArea coercion for consistent sub-area naming
  - ArcGIS geocoding integration
- **ID Conflict Resolution Modal**: New UI for resolving duplicate/conflicting ID field mappings
- **New Standard Fields**: Additional RESO field definitions and updates
- **Duplicate Source Mapping Detection**: Warns when the same source column is mapped to multiple targets
- **SqFt Combination**: Combine multiple living area square footage fields (e.g., 1st floor + 2nd floor = total)
- **SaleQtr as Default Derived Metric**: Quarterly sale date calculation included by default
- **Rapidfuzz Integration**: Faster fuzzy matching for column name suggestions
- **App Chooser Modal**: After login, users select between Column Mapper and Decision Trees applications
  - Visual workflow guide showing when to use each app
  - Clickable flowchart with L-shaped connectors
  - Automatic launch of Decision Trees on port 4000 with browser redirect
  - Sign out option from chooser screen
- **AGPL v3 License**: Project now licensed under GNU Affero General Public License v3.0
- **Distribution Build Script**: `build_release.R` creates clean zip for sharing

### Changed
- **Rebranding**: Changed "CValR Column Mapper" to "CAA Column Mapper" throughout UI
