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
  - Title bar now shows "CAA Column Mapper"
  - Welcome modal updated with new branding
  - Login screen now shows "CAA ClearValR Tools"
- **Default to Local-Only Mode**: ColumnMapper now defaults to local mode without requiring MongoDB

### Fixed
- **SaleQtr Crash**: Fixed crash on non-date values in SaleQtr calculation
- **Baths Split Auto-Accept**: Fixed baths split not auto-accepting on pre-loaded profiles
- **Preview Button Special Characters**: Use `document.getElementById` for dynamic UI preview buttons to handle special characters in column names
- **Analytics Charts Comma Numbers**: Fixed charts failing when numeric columns contain comma-formatted numbers
- **Exclusion Feature**: Auto-clear excluded selections on profile reload to prevent stale state
- **Misrouted Uploads**: Enforce rejection for uploads sent to the wrong handler
- **CValR Export Fixes**: Various fixes to export pipeline column ordering and filtering

---

## [2.3.1] - 2025-12-07

### Added
- **Include ListingId Option**: New checkbox in export options to preserve ListingId/MLS# column when using destination schema mode
  - Allows MLS numbers to pass through to Decision Trees for CMS sales identification
  - Enabled by default; uncheck to exclude ListingId from exports
  - Works even when destination schema doesn't include a ListingId column

---

## [2.3.0] - 2025-12-07

### Added
- **SaleQtr Derived Metric**: Automatically calculate Year.Quarter from date columns
  - Address-style clickable date column suggestions for SaleQtr field
  - Preview shows calculated Year.Quarter values
  - Fixed duplicate calculation bug that overwrote correct values
- **Address Component Auto-Suggest**: When mapping to Address field
  - Automatically detects unmapped address-related columns in source data
  - Shows clickable suggestions for: StreetDirPrefix, StreetDirSuffix, UnitNumber, City, StateOrProvince, PostalCode, PostalCodePlus4
  - Only shows suggestions that exist in source data and aren't already selected
- **MongoDB User Authentication**: Full authentication system
  - Login/registration UI with username, email, password
  - Password hashing with SHA-256 + salt
  - "Continue without account" local mode option
  - User session state with logout functionality
- **MongoDB Profile Storage** (backend ready):
  - `save_profile_to_db()`, `load_profile_from_db()` functions
  - `list_user_profiles()`, `delete_profile_from_db()` functions
  - Profiles stored as JSON in MongoDB

### Changed
- **Date Field Mapping Improvements**:
  - DateSold synonyms: added "sold date", "date sold", "closedate", etc.
  - DateListed synonyms: added "on market date", "onmarketdate", etc.
  - Fixed Stories/Levels synonym matching with fallback search
- **Upload Limit**: Increased to 50MB (was 5MB default)
- **Export Filtering**: Destination columns no longer excluded as "unclassified"
- **Workflow Wizard**: Now appears after login (not on top of login screen)

### Technical
- `.Renviron.example` template for MongoDB URI
- `.Renviron` added to .gitignore (secrets protection)
- mongolite, digest, shinyjs package dependencies

---

## [2.2.0] - 2025-12-07

### Added
- **RESO Data Dictionary Integration**:
  - Load RESO field definitions from JSON at startup
  - Build synonym lookup for improved field matching
  - `generate_reso_templates.R` utility script
  - Included `RESO_Data_Dictionary_Full.csv` and `.json` templates
- **Class 3 Mapping Review Workflow**:
  - Class 3 columns appear in mapping UI with orange "REVIEW" badge
  - Accept/Skip buttons for each Class 3 mapping card
  - Option to upgrade Class 3 to Class 2 or Class 1 when accepting
  - "Accept All Class 3" bulk action button with upgrade option
- **Export Class Filter**:
  - Checkboxes to select which classes (1, 2, 3) to include in export
  - Filter applies to both mapped columns and destination schema padding
  - Unchecked classes result in fewer columns (not empty columns)
- **Enhanced Classification Patterns**:
  - Add class attribute (1/2/3) to all fields in `logic_target_groups`
  - Build synonym-to-classification lookup from target groups
  - New Class 4 patterns: MLS internal codes (SAID, BAID, BRCD), agent/office shorthand, contact fields, showing/lockbox info
  - Normalized pattern matching (handles spaces in column names)

### Changed
- **Duplicate Column Handling**:
  - True duplicates (same data) silently removed at import
  - Conflicting duplicates (different data) reject the file
  - Removed post-import duplicate detection UI (Dups badge, filter)

### Fixed
- **Class 4 Export Bug**: Class 4 columns now fully excluded from destination schema exports (both header and data, not just data)
- **Destination Schema UX**: Hide "Available Target Fields Not Matched" section when using destination schema mode
- **Accept All Class 3 Crash**: Fixed NA == 3 comparison issue
- **Individual Accept Button Crash**: Fixed cache entry type handling for both list and atomic vector formats
- **Suggestion Filter**: Filter Class 4 columns from auto-suggestions
- **Reverse Match Bug**: Single-letter words no longer match too broadly

---

## [2.1.3] - 2025-12-03

### Fixed
- **Class 4 Export**: Columns now fully excluded from destination schema exports (both header and data)
- **Destination Schema UX**: "Available Target Fields Not Matched" section hidden when using destination schema mode

---

## [2.1.2] - 2025-11-17

### Added
- **Derived Metric Toggle:** New “Include derived metrics calculated by the app (PricePerSqFt, PricePerLotSF)” option beside the build-info checkbox lets users decide whether the auto-generated metrics are kept in the preview/export, even when a destination schema is restricting columns.

### Changed
- **Metric Naming & Detection:** PPSF/PPA columns were renamed to `PricePerSqFt`/`PricePerLotSF`, now rounded to the nearest dollar and only generated when source data lacks meaningful values. Field detection now understands `SqFt`, `LotSF`, and related synonyms so derivations run against the columns already present in your MLS feeds.
- **Workflow Respecting Toggle:** Both the mapped-data preview and CSV export pipelines remember which derived columns were created during the last apply and only drop those when the new toggle is unchecked, preventing user-supplied metrics from being removed.

---

## [2.1.1] - 2025-11-17

### Fixed
- **Mapped Data Preview Table**: Cleaned address text to strip embedded newlines/tabs and forced per-column nowrap with a 50-character cap so tall rows collapse to a single line.
- **Numeric Formatting**: Preview now formats all numeric columns as trimmed, non-scientific strings which keeps prices and IDs readable regardless of magnitude.

### Changed
- **Version Badge Resilience**: `format_build_label()` now falls back to environment-provided build metadata so the header/footer always display the version and Git SHA even when Git isn’t available (e.g., IDE crash recovery, Positron temp sessions).

---

## [2.1.0] - 2025-11-17

### Added
- **Derived Metrics Engine**: Automatically calculates Price Per Square Foot (PPSF) and Price Per Acre (PPA) whenever source fields (price, square footage, lot size) are present, with clear success/error logging.
- **Export Provenance Columns**: `.Mapper_Version`, `.Mapper_SHA`, `.Mapper_Tag`, and `.Mapper_Date` columns are appended to exported datasets so analysts know exactly which mapper build produced the file.
- **Analytics Integration**: Market visualizations now consume the derived PPSF column when available (and gracefully fall back to on-the-fly calculations), ensuring all plots reflect the latest metric logic.

### Changed
- **Mapping Workflow Messaging**: Console output now summarizes derived metric coverage (valid/invalid/gray-zone counts) to help diagnose data-quality issues quickly.

---

## [2.0.0] - 2025-11-05

### Added
- **Workflow Wizard**: Welcome screen asking if user has a profile ready
  - "Yes - Load my profile" workflow
  - "No - I'll create one now" workflow
  - Dynamic sidebar that adapts based on user's choice
- **Dark Mode**: Full dark theme support
  - Toggle switch in sidebar
  - Persistent preference via localStorage
  - Optimized styling for all UI components
  - Analytics plot descriptions readable in dark mode
- **Educational Analytics Section**: 5 market visualization plots
  - Sales Price Trend Over Time (linear + polynomial)
  - Price Distribution Histogram
  - Price Comparison by Neighborhood
  - Market Activity: Sales Volume Over Time
  - Price per Square Foot Analysis
  - Each plot includes educational description boxes explaining usefulness and limitations
- **Auto-Suggestions for Unmapped Fields**: After profile loads, remaining unmapped fields now get auto-suggestions
- **Blank Row Removal**: Automatically removes completely blank rows on CSV upload with notification
- **Version Display**: Footer showing current version and last updated date
- **Enhanced Documentation**: Comprehensive README with workflow examples and troubleshooting

### Changed
- **Profile-First Workflow**: Restructured UI to support loading profile before or after CSV
- **Badge Logic Enhancement**: Clearer distinction between profile mappings (green), suggestions (yellow), and missing (red)
- **Value Source Tracking**: Internal tracking system to differentiate mapping origins (profile, user, suggestion)
- **UI Refresh Mechanism**: Added triggers to ensure UI updates properly after profile loads
- **README Structure**: Complete rewrite with step-by-step workflows and real-world examples

### Fixed
- **Badge Color Bug**: Fixed issue where fields showed GREEN "Mapped: X" when not actually in the profile
- **Dark Mode Text Readability**: Fixed light gray text on light gray background in preview popups
- **Profile Mapping Persistence**: Improved profile loading to properly track which mappings succeeded
- **UI Sync Issues**: Fixed cases where badges didn't update after profile loaded

### Technical
- Added `APP_VERSION` and `APP_VERSION_DATE` constants
- Implemented CSS class-based styling (reduced inline styles)
- Enhanced reactive value tracking with `value_source` reactiveVal
- Improved localStorage integration for dark mode preference
- Optimized analytics plot rendering with proper data validation

---

## [1.0.0] - 2025-11-03

### Added
- **Core Mapping Interface**: Column-to-field mapping with dropdown selectors
- **Auto-Suggestion Algorithm**: Multi-criteria scoring system
  - Exact match (100 points)
  - Synonym match (90 points)
  - Word match (80 points)
  - Partial match (50 points)
  - Data inspection (20 points)
  - Minimum threshold: 70 points
- **Profile Management**: Save and load mapping profiles
  - Support for CSV, JSON, and RDS formats
  - Reusable profiles for different MLS systems
- **Visual Feedback System**: Color-coded badges
  - Green: Mapped fields
  - Yellow: Suggested fields
  - Red: Missing fields
- **Data Preview**: Eye icon (👁️) to preview column data
- **Unmapped Columns Management**: UI to include/exclude unmapped columns
- **Standard Field Definitions**: 52+ standardized fields
  - Date fields (4)
  - Price fields (3)
  - Location fields (8)
  - Identifiers (6)
  - Property attributes (26)
  - Basement & features (3)
  - Remarks & notes (2)
- **Date Conversion**: Automatic conversion to YYYY-MM-DD format
- **Data Table Preview**: Interactive DT table with search and sort

### Technical
- Built with R Shiny
- Dependencies: shiny, shinyWidgets, DT, jsonlite
- Single-file architecture (~2500 lines) for portability
- Reactive programming for real-time updates
- Optimized for large datasets (tested up to 50K rows)

---

## Version Numbering Strategy

This project uses [Semantic Versioning](https://semver.org/):

**Format:** MAJOR.MINOR.PATCH

- **MAJOR**: Breaking changes or major feature overhauls
  - Example: Changing from single-file to modular architecture
  - Example: Integration into main CValR app
- **MINOR**: New features or significant enhancements
  - Example: Adding workflow wizard
  - Example: Adding dark mode
  - Example: Adding analytics section
- **PATCH**: Bug fixes and minor improvements
  - Example: Fixing badge color logic
  - Example: Improving dark mode contrast

### When to Increment

**MAJOR (X.0.0):**
- Removing features
- Changing profile format in non-compatible way
- Major architectural refactor
- Integration that changes standalone behavior

**MINOR (0.X.0):**
- Adding new features (workflow wizard, dark mode, analytics)
- New standard fields
- New export formats
- Significant UI enhancements
- New auto-suggestion scoring criteria

**PATCH (0.0.X):**
- Bug fixes
- UI polish
- Performance improvements
- Documentation updates
- Minor styling adjustments

### Example Release Timeline

- **v1.0.0** (2025-11-03): Initial release
- **v2.0.0** (2025-11-05): Major feature additions (workflow wizard, dark mode, analytics)
- **v2.1.0** (2025-11-17): Added PricePerSqFt/PricePerLotSF derived metrics plus export provenance columns
- **v2.1.1** (2025-11-17): Polished mapped-data preview table and hardened version badge display
- **v2.1.2** (2025-11-17): Added derived metric toggle, better field detection, and rounded PricePerSqFt/LotSF outputs
- **v2.1.3** (2025-12-03): Class 4 export fix and destination schema UX improvements
- **v2.2.0** (2025-12-07): RESO integration, Class 3 review workflow, export class filter, CRMLS MVP
- **v2.3.0** (2025-12-07): MongoDB authentication, address auto-suggest, SaleQtr derived metric
- **v2.3.1** (2025-12-07): Include ListingId option for destination schema exports
- **v2.4.0** (2026-02-25): Synonym editing, decimal baths split, data enrichment, rapidfuzz, CAA rebranding, AGPL v3 license


---

## How to Update Version

When releasing a new version:

1. **Determine version number** using Semantic Versioning rules above

2. **Update version constants** in `column_mapper.R`:
   ```r
   APP_VERSION <- "X.Y.Z"
   APP_VERSION_DATE <- "YYYY-MM-DD"
   ```

3. **Update CHANGELOG.md**:
   - Move items from [Unreleased] to new version section
   - Add release date
   - Create new empty [Unreleased] section

4. **Update README.md** version footer:
   ```markdown
   **Version:** X.Y.Z
   **Last Updated:** YYYY-MM-DD
   ```

5. **Create git tag**:
   ```bash
   git tag -a vX.Y.Z -m "Release version X.Y.Z"
   git push origin vX.Y.Z
   ```

6. **Create GitHub Release** (if applicable):
   - Use tag created above
   - Copy changelog entry as release notes
   - Attach compiled artifacts if needed

---

## Links

- [GitHub Repository](https://github.com/CAA-EBV-CO-OP/CAADataBridge)
- [Issue Tracker](https://github.com/CAA-EBV-CO-OP/CAADataBridge/issues)
- [README](README.md)
- [Semantic Versioning](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/)
