# CValR Column Mapper

*Distributed as **CAADataBridge** by the Community of Asset Analysts (CAA)*

A standalone Shiny app for mapping CSV columns to standardized field names for real estate data analysis.

## Version

**Version:** 2.4.0

## Important: This is a Local Application

**This app runs on YOUR computer** - it does not connect to any online service. This means:

- All mapping profiles are saved to **YOUR local hard drive**
- You must remember where you save your mapping files (we recommend creating a dedicated folder)
- When loading a profile, you'll browse to find it on your computer
- Your data never leaves your computer - completely private and secure

**Recommended Setup:** Create a folder on your computer to store mapping profiles, for example:
- Windows: `C:\Users\YourName\Documents\CValR_Mappings\`
- Mac/Linux: `~/Documents/CValR_Mappings/`

## Purpose

Real estate data comes in many formats from different MLS systems. This tool helps you:
- Map your CSV columns to CValR's standard field names or alternate schemas such as the RESO standard
- Save mapping profiles to your local computer for reuse with the same data source
- Preview data to verify correct mappings
- Handle unmapped columns (include or exclude)
- Export clean, standardized datasets

## License & Community

This project is licensed under the **GNU Affero General Public License v3.0** (AGPL-3.0).
See [LICENSE](LICENSE) for the full text.

**What this means for you:**
- You are free to use, modify, and share this software
- If you modify it and make it available over a network, you must share your source code
- Any derivative works must also be licensed under AGPL-3.0

**A note from the Community of Asset Analysts (CAA):**
This tool was built by and for the real estate analyst community. While the AGPL permits
redistribution, we encourage you to share improvements back through the official CAA
channels rather than creating independent forks. Community contributions make the tools and the community better for everyone. If you've built something useful on top of this, we'd love to hear
about it!

---
## Features

✅ **Workflow Wizard** - Choose whether to load an existing profile or start fresh
✅ **Smart Auto-Suggestions** - Intelligent matching based on column names and data content
✅ **Visual Feedback** - Color-coded badges show mapping status
✅ **Data Preview** - Click eye icons to see sample values
✅ **Profile Management** - Save/load mappings in RDS, CSV, or JSON format to your local drive
✅ **Unmapped Columns** - Control which extra columns to keep or exclude
✅ **Date Transformation** - Automatically converts dates to yyyy-mm-dd format
✅ **Dark Mode** - Toggle between light and dark themes for comfortable viewing
✅ **Analytics Visualizations** - Educational real estate market analysis plots
✅ **Destination Schema Projection** - Optionally upload a target dataset so the mapper outputs your data in that exact column layout (destination remains read-only)

## Quick Start

### Step 1: Launch the App

**From R console or RStudio:**
```r
shiny::runApp('C:/GitRepos/CValR_4.0/ColumnMapper/column_mapper.R')
```

**Or from command line:**
```bash
Rscript -e "shiny::runApp('C:/GitRepos/CValR_4.0/ColumnMapper/column_mapper.R', port=4545)"
```

**Using the launcher script (easiest):**
```bash
Rscript launch_mapper.R
```

### Step 2: Choose Your Workflow

When the app opens, you'll see a **welcome screen** asking:

> **"Do you have a mapping profile previously created with CValR Column Mapper ready to upload?"**

Click one of these buttons:

- **"Yes - Load my profile"** → Go to [Workflow A](#workflow-a-i-have-a-saved-profile)
- **"No - I'll create one now"** → Go to [Workflow B](#workflow-b-first-time--no-profile)

---

## Workflow A: I Have a Saved Profile

**Use this if:** You've mapped data from this MLS system before and saved a profile to your computer.

### Steps:

**1. Load Your Mapping Profile**
   - The sidebar shows **"1. Load Mapping Profile"** at the top
   - Click **"Choose File"** and browse to where you saved your profile (e.g., `C:\Users\YourName\Documents\CValR_Mappings\MLSPIN_profile.csv`)
   - Select your profile file (.csv, .rds, or .json)
   - The app loads your saved column mappings

**2. Upload Your CSV Data**
   - Scroll down to **"2. Data Upload"**
   - Click **"Upload Your CSV"** and select your real estate data file
   - The app automatically applies your saved mappings to the data
   - All matched columns show **GREEN badges** like "Mapped: SalePrice"
   - Any columns that don't match get **YELLOW auto-suggestions** or **RED missing** badges

**3. Review and Adjust**
   - Check that the green-badged mappings look correct
   - For any yellow suggestions, click the 👁️ eye icon to preview the data
   - Use the dropdown to change any incorrect mappings
   - Click **"Update Unmapped Columns"** to see columns that weren't mapped
   - Check/uncheck which unmapped columns to keep

**4. Finalize and Download**
   - Click **"Apply Mappings & Finalize"**
   - Preview your transformed data in the table
   - Review the analytics charts (price trends, distributions, etc.)
   - Click **"Download Mapped CSV"** to save your clean data

**5. Save Updated Profile (Optional)**
   - If you made changes, save the updated mapping profile
   - Scroll to **"3. Save/Export Mappings"**
   - Choose format (CSV recommended for compatibility)
   - Click **"Save Mapping Profile"**
   - Save to your mappings folder (e.g., `MLSPIN_profile_updated.csv`)

---

## Workflow B: First Time / No Profile

**Use this if:** This is your first time mapping data from this MLS system, or you don't have a saved profile.

### Steps:

**1. Upload Your CSV Data**
   - The sidebar shows **"1. Data Upload"** at the top
   - Click **"Upload Your CSV"** and select your real estate data file
   - The app reads your file and shows you the column mappings section

**2. Review Auto-Suggestions**
   - The app automatically suggests mappings based on column names
   - You'll see three types of badges:
     - 🟡 **Yellow "Suggested: [Column]"** - The app thinks this is the right match (verify it!)
     - 🟢 **Green "Mapped: [Column]"** - You've confirmed this mapping
     - 🔴 **Red "Missing"** - No match found (you'll need to select manually)

**3. Verify and Adjust Mappings**
   - **For yellow suggestions:** Click the 👁️ eye icon to preview the actual data
     - Does "Suggested: ClosePrice" actually contain sale prices?
     - If YES: Great! You can leave it or click the dropdown to confirm
     - If NO: Use the dropdown to select the correct column
   - **For red missing fields:** Click the dropdown and select the correct column from your data
   - **Tip:** You don't need to map every field - only the ones you need

**4. Handle Unmapped Columns**
   - Click **"Update Unmapped Columns"** button
   - This shows all the columns from your CSV that you didn't map
   - Check the boxes for any extra columns you want to keep (like custom MLS fields)
   - Uncheck columns you don't need (they'll be excluded)
   - Unmapped columns keep their original names if you include them

**5. Apply and Download**
   - Click **"Apply Mappings & Finalize"**
   - Preview your transformed data in the table below
   - Review the analytics charts to understand your data
   - Click **"Download Mapped CSV"** to save your clean, standardized data

**6. Save Your Mapping Profile** ⭐ **IMPORTANT!**
   - You'll see **"2. Save Your Mappings!"** highlighted in yellow
   - This is your chance to save all the work you just did!
   - Choose format:
     - **CSV** (recommended) - Works everywhere, easy to view/edit
     - **JSON** - Good for technical users
     - **RDS** - R-specific format
   - Click **"Save Mapping Profile"**
   - **Save it to a folder you'll remember!** (e.g., `C:\Users\YourName\Documents\CValR_Mappings\MLSPIN_profile.csv`)
   - **Name it clearly** (e.g., `MLSPIN_profile.csv`, `NEREN_August2024.csv`)

**Next time you work with data from this MLS system, you can load this profile and skip all the manual mapping!**

---

## Understanding the Color-Coded Badges

The badges tell you the status of each mapping:

| Badge | What It Means | What To Do |
|-------|---------------|------------|
| 🟢 **Green "Mapped: ColumnName"** | This mapping is confirmed (either from your profile or your manual selection) | Nothing - it's good to go! |
| 🟡 **Yellow "Suggested: ColumnName"** | The app thinks this is the right column, but you should verify it | Click 👁️ to preview the data and confirm it's correct |
| 🔴 **Red "Missing"** | No mapping found for this field | Click the dropdown to select the correct column, or leave it unmapped if you don't have this data |

## Saving and Managing Mapping Profiles

### Why Save Profiles?

Once you've mapped columns for a particular MLS system or data source, **save that work!** A saved profile means:
- ✅ Never map the same columns twice
- ✅ Process new data from the same source in seconds
- ✅ Share profiles with colleagues who use the same MLS system
- ✅ Maintain consistency across multiple data imports

### How to Save

1. After you've created or adjusted your mappings, scroll to the **Save/Export** section
2. Choose your format:
   - **CSV** (Recommended) - Human-readable, works everywhere, easy to edit in Excel
   - **JSON** - Structured format, good for technical users
   - **RDS** - R-specific binary format (fastest but only works in R)
3. Click **"Save Mapping Profile"**
4. Your browser will ask where to save the file
5. **Choose a dedicated folder** like `C:\Users\YourName\Documents\CValR_Mappings\`
6. **Name it clearly** to identify the data source: `MLSPIN_2024.csv` or `NEREN_profile.json`

### Where Are Profiles Stored?

**Important:** Mapping profiles are saved to **your local computer only** - wherever you choose to save them. This app does not store anything online or in the cloud.

**Best Practice:**
- Create a dedicated folder for all your CValR mapping profiles
- Use consistent naming: `[MLS_System]_[Date/Version].csv`
- Keep backups of important profiles

### Loading Saved Profiles

Next time you use the app:
1. When the welcome screen appears, click **"Yes - Load my profile"**
2. Click **"Choose File"** in the sidebar
3. Browse to where you saved your profile
4. Select the file and it loads instantly
5. Upload your new CSV data and the mappings apply automatically!

### Sharing Profiles

You can share mapping profiles with colleagues:
- Email them the .csv or .json file
- Store them in a shared network folder
- Everyone using the same MLS system can use the same profile!

# Merge Mapper CValR

Need to stitch together a primary export with secondary comps data before mapping? Use the **Merge Mapper** helper:

1. Run `Rscript merge_mapper.R` (or `source("merge_mapper.R")`) to launch the merge-only app. Alternatively, run `Rscript launch_mapper.R` and answer “yes” when prompted to open Merge Mapper first. Set `CVALR_MERGE_MODE=merge` to auto-launch it every time, or `CVALR_MERGE_MODE=skip` to suppress the prompt.
2. Upload the primary dataset on the left and the secondary dataset (e.g., Top 25 comps) on the right. Both CSV and Excel files are supported.
3. Pick the join keys for each file. Select a single column (like MLS) or make a composite by choosing multiple columns (Address + DateSold, PID + CloseDate, etc.). The tool normalizes casing and whitespace so matches are resilient.
4. Choose which columns to bring over from the secondary data, set your preferred join type (left/inner/full/right), and click **Run merge**. A summary shows matched vs unmatched rows, plus column counts.
5. Check the data-quality panel: duplicate join keys (both datasets), coverage of every appended column, rule-based warnings (bad dates, <=0 prices, lat/lon out of range), and samples where primary vs secondary values disagree. Fix issues upstream or retry with different columns as needed.
6. (Optional) Under **Derived Columns**, instantly add helper fields like `SaleQtr` (year.quarter derived from your chosen sale-date column) or `Address_Merged` (concatenates multiple address components). More quick-hit derived options will land here over time.
7. Preview the merged table and click **Download merged CSV**. The export uses the same RFC 4180–compliant writer as ColumnMapper, so multiline Notes fields, commas, and quotes stay intact.
8. Close the Merge Mapper window to return to the regular ColumnMapper launcher, then load the merged CSV just like any other dataset.

👉 Tip: keep the merge columns list concise (e.g., Notes, Design Style, Condition) to avoid recreating duplicates of every column from the secondary file.

## Standard Field Names

The mapper uses 52+ standardized fields organized into groups:

### Date Fields
- `DateSold`, `DateListed`, `DatePending`, `OffMarketDate`

### Price Fields
- `PriceSold`, `PriceListed`, `PriceOriginal`, `PricePerSqFt`

### Location
- `Address`, `City`, `PostalCode`, `Latitude`, `Longitude`, `Neighbourhood`, `Region`

### Property Attributes (26 fields)
- Bedrooms, bathrooms, square footage, lot size, year built, etc.

**See full field list:** [MAPPING_INTELLIGENCE_ROADMAP.md](MAPPING_INTELLIGENCE_ROADMAP.md)

## Using Destination Schemas

Need to deliver into a partner's template instead of the default CValR layout? Use the **Destination Schema** workflow:

1. In the sidebar, check **"Use destination schema"** and upload the destination dataset (CSV/Excel). Only the column headers are used—the data remains read-only.
2. The mapping grid now shows each destination column. Expand **“Destination Column Bindings”** to review or adjust which underlying CValR standard field powers every destination header—unassigned headers are highlighted so you can choose the best fit without hunting through the full schema.
3. Once you're confident in those bindings, click **"Promote destination headers to synonyms"** to add the destination header names into the suggestion pool. Future uploads will prioritize those labels when auto-matching source columns.
4. Map your source columns as usual. The 👁️ preview button is disabled until a source column is selected, preventing empty popups.
5. Optional: in the export controls (next to Download), toggle **"Restrict output to destination columns only"** if you want just the destination layout (derived metrics still respect their own toggle). Switch it off when you need the destination columns followed by every other mapped column or pass-through selection.
6. Click **"Apply Mappings & Finalize"**. The export is projected into the destination column order, missing destination columns are padded with blanks, and your chosen pass-through columns follow afterward.

👉 Tip: keep the destination dataset small (header-only sample works) so it loads quickly, and reuse it via mapping profiles to avoid re-uploading.

## Tips & Best Practices

### Verifying Auto-Suggestions

Yellow "Suggested" badges mean the system found a likely match, but you should verify:
- Click the eye icon to preview the data
- Make sure it's the right column
- If correct, leave it (or click dropdown to confirm and turn it green)

### Common Mapping Patterns

| Your Column | Maps To | Notes |
|-------------|---------|-------|
| Sold Date, Close Date | `DateSold` | Auto-detected |
| Sale Price, Close Price | `PriceSold` | Auto-detected |
| List Date, On Market | `DateListed` | Auto-detected |
| Bedrooms Total | `Beds` | May need manual selection |
| Bathrooms Full | `BA` | May need manual selection |
| Bathrooms Half | `PB` | May need manual selection |

### Handling Multiple Address Fields

If your data has separate address components:
- Map all to `Address` field (it accepts multiple selections)
- They'll be merged: "123 Main St, Unit 4"

### Date Format Issues

The app tries multiple date formats automatically:
- MM/DD/YYYY
- YYYY-MM-DD
- DD/MM/YYYY
- And more...

All dates are converted to `YYYY-MM-DD` in the output.

### Working With Assistants (Positron, VS Code, etc.)

Assistant tools sometimes run in isolated containers or temp clones that can become stale. To avoid confusion:

- Always start assistants from your local, up‑to‑date checkout (the same folder you work in).
- After pushing new commits, restart the assistant or run `git fetch --all --prune && git pull` in its terminal.
- When asking an assistant to clone, prefer pinning to a commit SHA or release tag (e.g., `v2.0.0`) instead of tracking `main`.
- This app surfaces repo freshness:
  - The launcher prints a `[Repo Status]` line showing whether you're ahead/behind/diverged from `origin/main`.
  - On app load, a toast appears if the repo is behind/diverged, and an informational toast if it's ahead or up‑to‑date.
- If you see behavior that looks “old,” check the build label/commit shown in the UI footer and compare to GitHub.


## Complete Workflow Examples

### Example 1: First-Time User (No Profile)

**Scenario:** You just downloaded 6 months of MLSPIN sales data with 127 columns.

1. **Launch app** → Welcome screen appears
2. **Click** "No - I'll create one now"
3. **Upload** `MLSPIN_Sales_Jan_June_2025.csv` (127 columns, 5,247 rows)
4. **Review** auto-suggestions: 38 fields automatically matched (yellow badges)
5. **Verify** suggestions by clicking 👁️ eye icons to preview data
6. **Fix** 4 incorrect suggestions using dropdowns
7. **Map** 3 red "Missing" fields manually
8. **Update unmapped** → 82 columns weren't mapped to standard fields
9. **Select** 8 extra columns to keep (like "SchoolDistrict", "TaxID")
10. **Apply** mappings → Final dataset has 49 columns (41 standard + 8 extras)
11. **Review** analytics charts to spot any data issues
12. **Download** clean CSV: `MLSPIN_cleaned.csv`
13. **Save profile** as `C:\Users\John\Documents\CValR_Mappings\MLSPIN_profile.csv`

**Time spent:** ~15 minutes for first-time mapping

### Example 2: Returning User (With Saved Profile)

**Scenario:** Same user downloads next month's MLSPIN data.

1. **Launch app** → Welcome screen appears
2. **Click** "Yes - Load my profile"
3. **Load profile** → Browse to `C:\Users\John\Documents\CValR_Mappings\MLSPIN_profile.csv`
4. **Upload** `MLSPIN_Sales_July_2025.csv` (129 columns, 892 rows)
5. **Auto-applied** → 41 fields mapped instantly with green badges
6. **Review** → All mappings correct (MLSPIN format hasn't changed)
7. **Update unmapped** → 2 new columns added by MLS, decide not to include them
8. **Apply** mappings → Same 49 column structure as before
9. **Download** clean CSV: `MLSPIN_July_cleaned.csv`

**Time spent:** ~2 minutes (just verification and download!)

### Example 3: New MLS System

**Scenario:** You start working with NEREN data instead of MLSPIN.

1. **Launch app** → Welcome screen
2. **Click** "No - I'll create one now" (different MLS = different column names)
3. **Upload** NEREN data
4. **Map columns** (different from MLSPIN, so auto-suggestions will vary)
5. **Save new profile** as `NEREN_profile.csv`
6. **Next time** with NEREN data → Load `NEREN_profile.csv` and you're done in 2 minutes!

**Strategy:** Build a library of profiles - one for each MLS system you work with!

## Troubleshooting

### Installation Issues

**Problem:** App won't load or shows package errors

**Solution:** Make sure all required packages are installed:
```r
install.packages(c("shiny", "shinyWidgets", "DT", "jsonlite", "ggplot2", "scales"))
```

If you still get errors, try updating all packages:
```r
update.packages(ask = FALSE)
```

### Workflow Issues

**Problem:** The welcome wizard doesn't appear

**Solution:** The wizard only shows once per session. If you need to change your workflow choice:
- Refresh the app (close and reopen it)
- The welcome screen will appear again

**Problem:** I chose "Yes - Load my profile" but I don't have one yet

**Solution:**
- Refresh the app (close and reopen)
- When the welcome screen appears, click "No - I'll create one now"
- Proceed to upload your CSV and create a profile

**Problem:** I can't find my saved mapping profile

**Solution:**
- Check your Downloads folder - that's where browsers save files by default
- Next time, create a dedicated folder like `C:\Users\YourName\Documents\CValR_Mappings\`
- When saving, use "Save As" to choose the location instead of just clicking Save
- **Tip:** Keep a text file in that folder listing what each profile is for

**Problem:** My profile loaded but nothing mapped

**Solution:**
- Check if the column names in your new CSV exactly match the profile
- If the MLS system changed their column names, you may need to create an updated profile
- Load the old profile, make adjustments, and save as a new version

### Mapping Issues

**Problem:** Auto-suggestions are poor or completely wrong

**Solution:** This is normal for:
- Unique/custom column names that don't match standard conventions
- MLS systems with unusual naming patterns
- **What to do:** Manually select the correct columns using the dropdowns - it's faster than you think!

**Problem:** Can't see data in the preview popup

**Solution:** Make sure you:
1. Have uploaded a CSV file
2. Selected a column from the dropdown first
3. Then clicked the 👁️ eye icon
- If preview still doesn't show, the column might be completely empty in your data

**Problem:** Downloaded CSV is empty or incomplete

**Solution:**
- Make sure you clicked **"Apply Mappings & Finalize"** before downloading
- Check that you actually mapped or included some columns
- If you excluded all unmapped columns, you might have too few columns left

**Problem:** Analytics charts show errors or "Missing required fields"

**Solution:**
- Charts require specific mapped fields (DateSold, PriceSold, etc.)
- Make sure you've mapped these fields in your column mapping
- Click "Apply Mappings & Finalize" first - charts only show after this step

### Dark Mode Issues

**Problem:** Dark mode text is hard to read

**Solution:**
- We've optimized dark mode styling
- If something looks wrong, try toggling dark mode off and on again
- Refresh the app if the theme doesn't apply correctly

## Future Enhancements

See [MAPPING_INTELLIGENCE_ROADMAP.md](MAPPING_INTELLIGENCE_ROADMAP.md) for planned features:
- Crowdsourced learning database
- MLS system auto-detection
- Profile repository
- Machine learning suggestions

## Technical Details

- **Built with:** R Shiny, shinyWidgets, DT, jsonlite, ggplot2, scales
- **Matching algorithm:** Multi-criteria scoring (exact match, synonyms, partial words, data inspection)
- **Performance:** Optimized for large datasets (1000+ rows, 100+ columns)
- **UI Features:**
  - Dynamic workflow wizard for guided user experience
  - Persistent dark mode preference (localStorage)
  - Real-time badge updates showing mapping status
  - Educational analytics with 5 market visualization plots
  - Automatic blank row removal on upload
- **Derived Metrics & Provenance:** Automatically calculates PricePerSqFt and PricePerLotSF (when Price/SqFt/LotSF exist) and appends mapper build metadata (.Mapper_Version/.Mapper_SHA/.Mapper_Tag/.Mapper_Date) to every export.
- **Data Privacy:** 100% local processing - no data sent to external servers

## Integration with CValR

This tool is designed to prepare data for the main CValR application. Once your data is mapped and downloaded, import it into CValR for analysis.

**Note:** Future versions will integrate this mapper directly into the CValR app for seamless workflow.

---

## Version Management

This project follows [Semantic Versioning](https://semver.org/).

**Current Version:** 2.1.2

For detailed version history and release notes, see:
- **[CHANGELOG.md](CHANGELOG.md)** - Complete version history with all changes
- **[VERSION.md](VERSION.md)** - Version management guide and release process

---

## Quick Reference Card

### First Time?
1. Launch app → Click "No - I'll create one now"
2. Upload CSV → Review auto-suggestions
3. Adjust mappings → Apply & Download
4. **Save your profile!** (you'll thank yourself later)

### Have a Profile?
1. Launch app → Click "Yes - Load my profile"
2. Load profile → Upload CSV
3. Verify → Apply & Download
4. Done in 2 minutes!

### Where Are My Files?
- **Mapping profiles:** Saved to wherever you choose (recommend: `C:\Users\YourName\Documents\CValR_Mappings\`)
- **Output data:** Downloaded to your browser's download folder
- **Nothing stored online** - it's all local!

---


**Last Updated:** 2026-02-25
**Maintainer:** CValR Development Team / Community of Asset Analysts (CAA)

**Changelog v2.1.x:**
- Added automatic PricePerSqFt/PricePerLotSF metrics with a user-facing include/exclude toggle plus `.Mapper_*` provenance columns in every export.
- Updated analytics visuals to use derived PricePerSqFt (with smart fallbacks) for more consistent trends.
- Tightened Mapped Data Preview column sizing, ellipsis behavior, numeric formatting, and derived-metric rounding for readability.
- Hardened the version badge so the exact build (version, SHA, tag) always displays in the UI footer/header.

**Changelog v2.4.0:**
*Notes see also Git commit summaries
- Added user-facing feature for editing destination column synonyms for both standard and alternate schemas.
Baths logic expanded to handle decimal system for bathrooms eg. 2.2= 2 Full Baths and 2 Half Baths 
or aka 2FB 2PB as referenced in the CvalR standard schema (comes from Eagle SFR George Dell course data)
Two modes:

  1. Standard/RESO Mode (no destination template): Map your source to "Baths (Decimal)"
   → click "Accept RESO Conversion" → export creates BathroomsFull, BathroomsHalf, and
  BathroomsTotalInteger columns.
  2. Destination Schema Mode (with RESO template loaded): Map decimal baths source to
  any bath field → accept conversion → populates all three RESO bath columns in the
  destination template.

  Both modes share the same baths_split_accepted state and split_decimal_baths()
  function. The auto-accept fix we just made also ensures this works automatically when
   loading a profile that previously accepted the conversion.

  The full documentation is in docs/DECIMAL_BATHS_IMPLEMENTATION.md
-Add ability to combine sqft for living areas eg. 1st floor sqft + 2nd floor sqft = total sqft.
-dded quarterly sale date calculation as default.