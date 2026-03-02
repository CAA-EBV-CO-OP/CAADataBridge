# CAACollaborative - Development Notes

## Critical: R Session Restart After Code Changes
**After ANY edit to `.R` files, the user MUST Restart R in RStudio (`Ctrl+Shift+F10`) before re-running the app.**

- Simply stopping the Shiny app and re-running does NOT reload sourced files. R caches functions and objects in memory.
- If a fix appears not to work, the FIRST thing to check is whether R was properly restarted.
- Do NOT layer additional fixes on top of a correct fix that "didn't work" — verify via restart first.
- Always remind the user of this step when delivering code changes.

## Project Structure
- Modular Shiny app: host navbar + two module tabs (Column Mapper, Decision Trees)
- See `R/host/` for top-level UI/server, `R/modules/` for each module
- Shared utilities in `R/shared/`
- All modules use Shiny's NS() / moduleServer() pattern — dynamic IDs in renderUI must use `ns()` or `session$ns()`

## Data Files
- Never commit CSV/MLS data files (enforced by .gitignore)
- `templates/` CSVs are an exception (schema templates)
- `sample_data/` is gitignored

## Shiny Module Namespace Reminders
- Every `inputId` and `outputId` inside `renderUI()` must be wrapped with `ns()`
- JavaScript that references Shiny input IDs must inject the namespace prefix
- Cross-module navigation uses `session$userData$root_session`
