# CValR Column Mapper

*Distributed as **CAADataBridge** for the [Community of Asset Analysts (CAA)](https://www.valuemetrics.info/community-of-asset-analysts)*

A standalone Shiny app for mapping CSV columns to standardized field names for real estate data analysis.

[![Lint R Code](https://github.com/CAA-EBV-CO-OP/CAADataBridge/actions/workflows/lint.yml/badge.svg)](https://github.com/CAA-EBV-CO-OP/CAADataBridge/actions/workflows/lint.yml)
[![Shiny App Smoke Test](https://github.com/CAA-EBV-CO-OP/CAADataBridge/actions/workflows/shinytest2.yml/badge.svg)](https://github.com/CAA-EBV-CO-OP/CAADataBridge/actions/workflows/shinytest2.yml)


## Version

**Version:** 2.4.0

## Quick Start

### 1. Open the project

Open this folder in RStudio:

`C:\Users\pryac\PR-Repo\CAACollaborative`

### 2. Launch the app

In the R console:

```r
source("launch.R")
```

`launch.R` checks required packages and tells you exactly which ones to install if anything is missing.

### 3. Use the recommended mapping flow

- If mapping to **CValR standard fields**: upload MLS data first and map normally.
- If mapping to a **custom destination schema**: load the destination schema first, then upload MLS data.

This keeps mapping focused on your chosen destination layout from the start.

### 4. Data Flow (Step-by-Step)

#### Standard CValR flow

1. Upload your MLS CSV/Excel file.
2. Review auto-suggestions and adjust mappings.
3. Apply mappings and export.

#### Custom destination schema flow

1. Check **Use destination schema**.
2. Upload or select your destination schema/template first.
3. Upload your MLS CSV/Excel file.
4. Review destination-column mappings, then apply mappings and export.

Loading the destination schema first prevents the extra remap feeling and keeps mapping aligned to your selected output schema.

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
