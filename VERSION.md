# Version Management Guide

**Current Version:** 2.4.0
**Last Updated:** 2026-02-24

This document explains how versioning works for the CAA Column Mapper and how to manage version updates.

---

## Table of Contents

1. [Semantic Versioning](#semantic-versioning)
2. [Version Number Format](#version-number-format)
3. [When to Increment](#when-to-increment)
4. [Release Process](#release-process)
5. [Version Display Locations](#version-display-locations)
6. [Examples](#examples)

---

## Semantic Versioning

The CAA Column Mapper follows [Semantic Versioning 2.0.0](https://semver.org/).

**Why Semantic Versioning?**
- Clear communication of change impact
- Industry standard (used by npm, Python, Ruby, etc.)
- Helps users understand compatibility
- Automated tooling support

**Format:** `MAJOR.MINOR.PATCH`

```
2.0.0
│ │ │
│ │ └─── PATCH: Bug fixes, minor improvements
│ └───── MINOR: New features, backward compatible
└─────── MAJOR: Breaking changes, major overhauls
```

---

## Version Number Format

### MAJOR Version (X.0.0)

**Increment when:**
- Breaking changes to existing functionality
- Removing features
- Incompatible profile format changes
- Major architectural overhaul
- Changes that require users to modify their workflow

**Examples:**
- v1.0.0 → v2.0.0: NOT used (v2.0.0 is backward compatible)
- v2.0.0 → v3.0.0: Integration into main CAA ClearValR app (standalone version discontinued)
- v3.0.0 → v4.0.0: Complete UI redesign requiring relearning

**Note:** v2.4.0 will rebrand from "CValR" to "CAA" branding.

**Impact:** Users may need to:
- Update documentation/training materials
- Recreate mapping profiles
- Modify integration scripts
- Relearn portions of the interface

---

### MINOR Version (0.X.0)

**Increment when:**
- Adding new features
- Adding new standard fields
- Significant UI enhancements
- New export/import formats
- Enhanced algorithms (better suggestions)

**Examples:**
- v1.0.0 → v2.0.0: Added workflow wizard, dark mode, analytics (should have been v1.1.0 technically, but we called it v2.0.0 for marketing impact)
- v2.0.0 → v2.1.0: Add batch CSV processing
- v2.1.0 → v2.2.0: Add Excel export format
- v2.2.0 → v2.3.0: Add profile sharing/marketplace

**Impact:** Users get:
- New capabilities
- Enhanced existing features
- Better performance
- More options

**Compatibility:** Existing workflows continue to work

---

### PATCH Version (0.0.X)

**Increment when:**
- Bug fixes
- Performance improvements (no new features)
- UI polish (minor styling)
- Documentation updates
- Dependency updates (security)

**Examples:**
- v2.0.0 → v2.0.1: Fix badge color logic bug
- v2.0.1 → v2.0.2: Improve dark mode contrast
- v2.0.2 → v2.0.3: Fix preview popup race condition
- v2.0.3 → v2.0.4: Update documentation

**Impact:** Users get:
- Bug fixes
- Improved stability
- Better performance

**Compatibility:** No changes to functionality

---

## When to Increment

### Decision Tree

```
Did you change the code?
├─ No → No version change needed
└─ Yes
    │
    ├─ Does it break existing functionality?
    │  └─ Yes → MAJOR version (X.0.0)
    │
    ├─ Does it add new features?
    │  └─ Yes → MINOR version (0.X.0)
    │
    └─ Is it just a bug fix or polish?
       └─ Yes → PATCH version (0.0.X)
```

### Special Cases

**Documentation Only:**
- README updates → No version change (or PATCH if significant)
- CHANGELOG updates → Part of version release process
- Code comments → No version change

**Dependencies:**
- Security updates → PATCH
- Feature additions via new dependency → MINOR
- Dependency removal → MAJOR (if affects functionality)

**Performance:**
- Optimization (same functionality) → PATCH
- New algorithm (better results) → MINOR
- Algorithm change (different results) → MAJOR

**UI Changes:**
- Color/style tweaks → PATCH
- New UI sections/features → MINOR
- Complete redesign → MAJOR

---

## Release Process

### Step-by-Step Guide

#### 1. Decide Version Number

Use the decision tree above to determine new version number.

**Example:** Adding batch CSV processing
- Not breaking → Not MAJOR
- Adds new feature → MINOR
- Current: v2.0.0 → New: v2.1.0

---

#### 2. Update Code

**File:** `ColumnMapper/column_mapper.R`

Update constants at top of file:

```r
# Application Version (Semantic Versioning: MAJOR.MINOR.PATCH)
APP_VERSION <- "2.1.0"  # Change this
APP_VERSION_DATE <- "2025-11-15"  # Change this to today's date
```

**Lines:** ~10-11 in column_mapper.R

---

#### 3. Update CHANGELOG.md

**Move items from [Unreleased] to new version section:**

```markdown
## [Unreleased]

### Planned
- Future features...

---

## [2.1.0] - 2025-11-15

### Added
- Batch processing for multiple CSV files
- Progress indicator for large uploads

### Fixed
- Preview popup race condition

### Changed
- Improved suggestion algorithm performance
```

**Structure:**
- **Added**: New features
- **Changed**: Changes to existing features
- **Deprecated**: Features marked for removal
- **Removed**: Features that were removed
- **Fixed**: Bug fixes
- **Security**: Security fixes

---

#### 4. Update README.md

**File:** `ColumnMapper/README.md`

Update version footer at bottom:

```markdown
**Version:** 2.1.0
**Last Updated:** 2025-11-15
**Maintainer:** CValR Development Team

**Changelog v2.1.0:**
- Added batch processing for multiple CSV files
- Fixed preview popup race condition
- Improved suggestion algorithm performance
```

---

#### 5. Commit Changes

```bash
git add ColumnMapper/column_mapper.R
git add ColumnMapper/CHANGELOG.md
git add ColumnMapper/README.md
git commit -m "Release version 2.1.0

- Add batch CSV processing
- Fix preview popup race condition
- Improve suggestion performance

See CHANGELOG.md for full details"
```

---

#### 6. Create Git Tag

```bash
# Create annotated tag
git tag -a v2.1.0 -m "Release version 2.1.0

Batch processing support, bug fixes, and performance improvements.

See CHANGELOG.md for full release notes."

# Push commits
git push origin main

# Push tag
git push origin v2.1.0
```

**Why tags?**
- Mark specific points in git history
- Easy to find releases
- GitHub automatically creates releases from tags

---

#### 7. Create GitHub Release (Optional)

If using GitHub:

1. Go to repository → Releases → Draft a new release
2. Choose tag: v2.1.0
3. Release title: "CValR Column Mapper v2.1.0"
4. Description: Copy from CHANGELOG.md
5. Attach files (if needed):
   - `column_mapper.R` (standalone file)
   - `README.md`
   - Sample profiles
6. Click "Publish release"

---

#### 8. Announce (Optional)

- Update project documentation
- Notify users via email/Slack/Discord
- Post on social media if public project
- Update project website

---

## Version Display Locations

The version number appears in several places. Keep them in sync!

### 1. Code Constants

**File:** `ColumnMapper/column_mapper.R`
**Lines:** ~10-11

```r
APP_VERSION <- "2.0.0"
APP_VERSION_DATE <- "2025-11-05"
```

**Purpose:** Used by app UI footer

---

### 2. Code Comments

**File:** `ColumnMapper/column_mapper.R`
**Line:** ~4

```r
# Version: 2.0.0
```

**Purpose:** Quick reference for developers reading code

---

### 3. UI Footer

**File:** `ColumnMapper/column_mapper.R`
**Lines:** ~805-806

```r
sprintf("CValR Column Mapper v%s", APP_VERSION),
sprintf("Last Updated: %s", APP_VERSION_DATE)
```

**Purpose:** Displayed at bottom of app for users

---

### 4. README.md

**File:** `ColumnMapper/README.md`
**Bottom of file**

```markdown
**Version:** 2.0
**Last Updated:** 2025-11-05
```

**Purpose:** Documentation reference

---

### 5. CHANGELOG.md

**File:** `ColumnMapper/CHANGELOG.md`
**Version headers**

```markdown
## [2.0.0] - 2025-11-05
```

**Purpose:** Release history

---

### 6. Git Tags

**Command:** `git tag`

```
v1.0.0
v2.0.0
```

**Purpose:** Mark releases in git history

---

## Examples

### Example 1: Bug Fix Release

**Scenario:** Fixed dark mode contrast issue

**Current Version:** v2.0.0
**New Version:** v2.0.1 (PATCH)

**Changes:**

1. **column_mapper.R:**
   ```r
   APP_VERSION <- "2.0.1"
   APP_VERSION_DATE <- "2025-11-06"
   ```

2. **CHANGELOG.md:**
   ```markdown
   ## [2.0.1] - 2025-11-06

   ### Fixed
   - Dark mode text contrast in analytics info boxes
   - Preview popup styling in dark mode
   ```

3. **README.md:**
   ```markdown
   **Version:** 2.0.1
   **Last Updated:** 2025-11-06
   ```

4. **Git:**
   ```bash
   git commit -m "Release v2.0.1 - Dark mode contrast fixes"
   git tag -a v2.0.1 -m "Bug fix release: Dark mode improvements"
   git push origin main v2.0.1
   ```

---

### Example 2: New Feature Release

**Scenario:** Added batch processing for multiple CSVs

**Current Version:** v2.0.0
**New Version:** v2.1.0 (MINOR)

**Changes:**

1. **column_mapper.R:**
   ```r
   APP_VERSION <- "2.1.0"
   APP_VERSION_DATE <- "2025-11-15"
   ```

2. **CHANGELOG.md:**
   ```markdown
   ## [2.1.0] - 2025-11-15

   ### Added
   - Batch processing: Upload multiple CSV files at once
   - Progress indicator for batch operations
   - Summary report after batch processing

   ### Changed
   - Improved file upload UI to support multiple selections
   ```

3. **README.md:**
   ```markdown
   **Version:** 2.1.0
   **Last Updated:** 2025-11-15

   **Changelog v2.1.0:**
   - Added batch processing for multiple CSV files
   - Added progress indicator
   - Improved file upload interface
   ```

4. **Git:**
   ```bash
   git commit -m "Release v2.1.0 - Batch processing feature"
   git tag -a v2.1.0 -m "Feature release: Batch CSV processing"
   git push origin main v2.1.0
   ```

---

### Example 3: Breaking Change Release

**Scenario:** Integrate into main CValR app (no longer standalone)

**Current Version:** v2.5.0
**New Version:** v3.0.0 (MAJOR)

**Changes:**

1. **column_mapper.R:**
   ```r
   APP_VERSION <- "3.0.0"
   APP_VERSION_DATE <- "2026-01-15"
   ```

2. **CHANGELOG.md:**
   ```markdown
   ## [3.0.0] - 2026-01-15

   ### Changed
   - **BREAKING**: Integrated into main CValR app as a module
   - Standalone version discontinued
   - Now shares reactive values with main app tabs

   ### Migration Guide
   - Existing profiles remain compatible
   - Standalone users should use v2.x branch
   - See MIGRATION.md for integration details
   ```

3. **README.md:**
   ```markdown
   **Version:** 3.0.0
   **Last Updated:** 2026-01-15

   **⚠️ Breaking Changes:**
   - No longer standalone - integrated into CValR main app
   - For standalone version, see v2.x branch
   ```

4. **Git:**
   ```bash
   git commit -m "Release v3.0.0 - Integration into main app (BREAKING)"
   git tag -a v3.0.0 -m "Major release: Main app integration"
   git push origin main v3.0.0
   ```

---

## Quick Reference

### Checklist for New Release

- [ ] Determine version number (MAJOR.MINOR.PATCH)
- [ ] Update `APP_VERSION` in column_mapper.R
- [ ] Update `APP_VERSION_DATE` in column_mapper.R
- [ ] Update version comment at top of column_mapper.R
- [ ] Update CHANGELOG.md (move [Unreleased] items to new version)
- [ ] Update README.md version footer
- [ ] Test the app (verify version displays correctly)
- [ ] Commit changes with descriptive message
- [ ] Create git tag: `git tag -a vX.Y.Z -m "Release vX.Y.Z"`
- [ ] Push commits: `git push origin main`
- [ ] Push tag: `git push origin vX.Y.Z`
- [ ] Create GitHub release (optional)
- [ ] Announce release (optional)

---

## Version History Quick View

| Version | Date | Type | Highlights |
|---------|------|------|------------|
| v1.0.0 | 2025-11-03 | Initial | First stable release |
| v2.0.0 | 2025-11-05 | Minor (called Major) | Workflow wizard, dark mode, analytics |
| v2.1.x | 2025-11-17 | Minor/Patch | Derived metrics, preview improvements |
| v2.2.0 | 2025-12-07 | Minor | RESO integration, Class 3 review, CRMLS MVP |
| v2.3.0 | 2025-12-07 | Minor | MongoDB auth, address suggest, SaleQtr |
| v2.3.1 | 2025-12-07 | Patch | Include ListingId option for destination schema exports |
| v2.4.0 | Unreleased | Minor | App chooser, Decision Trees integration, CAA rebrand |
| v3.0.0 | Future | Major | Planned: Main app integration |

---

## Additional Resources

- [Semantic Versioning Specification](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/)
- [Git Tagging](https://git-scm.com/book/en/v2/Git-Basics-Tagging)
- [GitHub Releases](https://docs.github.com/en/repositories/releasing-projects-on-github)

---

**Maintainer:** CAA Development Team
**Last Updated:** 2025-12-07
