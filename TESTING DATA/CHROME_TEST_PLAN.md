# CAA Collaborative - Chrome Automation Test Plan

## Prerequisites
- App running at http://127.0.0.1:4545 (or whatever port RStudio assigns)
- Test files in `TESTING DATA/`:
  - `MLS Inputdata/test_200_mixed_status.csv` (200 rows, 50 cols)
  - `Destination Schema/woodGEO.csv` (266 rows, 20 cols)

## CSV Upload Strategy
Shiny's fileInput uses a native OS file picker that browser automation cannot control.
**Solution:** Use JavaScript injection to create a File object from the CSV content
and programmatically attach it to the file input element, then trigger Shiny's
change event. The CSV is read from disk via Bash, passed as a JS string literal.

---

## Test Cases

### T1: App Launch & Home Page
- Navigate to app URL
- Verify "Welcome to CAA Collaborative" heading appears
- Verify 3 app cards visible: Column Mapper, Decision Trees, Report Forms
- Verify Workflow Guide section visible
- Verify app starts in LIGHT mode (not dark)
- Screenshot

### T2: Navigate to Column Mapper
- Click "Column Mapper" button on home page
- Verify Column Mapper tab is active
- Verify upload controls are visible (file input, format selector)
- Screenshot

### T3: Upload MLS CSV Data
- Use JavaScript injection to upload test_200_mixed_status.csv
- Wait for processing (classification spinner)
- Verify "200 records loaded" or similar notification
- Verify column classification table populates
- Screenshot

### T4: Column Classifications
- Verify classification table shows columns with class 1/2/3/4
- Test filter buttons: click "Class 1" → only class 1 columns shown
- Click "Class 2" → class 2 columns shown
- Click all classes back on
- Screenshot of classification table

### T5: Auto-Mapping & Mapping Cards
- Verify mapping cards appear with auto-suggested mappings
- Verify selectize dropdowns are populated with source columns
- Verify cards show "Also known as:" synonyms where applicable
- Screenshot

### T6: Edit Synonyms (Pencil Icon) - NEW FIX
- Find a pencil icon next to a mapping card
- Click it
- Verify modal dialog opens with "Edit Synonyms for [field]"
- Verify text area is populated with current synonyms
- Verify Save and Cancel buttons are present
- Click Cancel to close
- Screenshot of modal

### T7: Apply Mappings
- Click "Apply Mappings & Finalize" button
- Verify notification appears confirming mappings applied
- Verify Mapped Data Preview table populates
- Screenshot

### T8: Mapped Data Preview Table - NEW FIX
- Verify the mapped data preview table is visible
- Verify horizontal scrollbar is visible and functional
- Verify table shows correct number of rows
- Scroll horizontally to verify all mapped columns appear
- Screenshot

### T9: Destination Schema Mode - NEW FIX
- Check "Use destination schema" checkbox
- Upload woodGEO.csv as destination schema (JS injection)
- Verify destination columns appear (DateSold, PriceSold, SqFt, etc.)
- Verify NO "object 'current_exclusions' not found" error
- Verify mapping cards render in destination mode
- Screenshot

### T10: Navigate to Decision Trees via Button
- Click the "Launch Decision Trees" button (or equivalent)
- Verify app switches to Decision Trees tab
- Verify notification about mapped data
- Screenshot

### T11: Decision Trees - Data Auto-Populated
- Verify data appears in the Decision Trees module
- Verify record count matches (200 or filtered subset)
- Verify response variable dropdown is populated
- Screenshot

### T12: Decision Trees - Build Tree
- Select response variable (e.g., SoldPrice or PriceSold)
- Click "Build Tree" or equivalent
- Wait for tree to render
- Verify decision tree visualization appears
- Screenshot

### T13: Dark Mode Toggle
- Click dark mode toggle switch
- Verify app switches to dark theme
- Verify text is still readable
- Toggle back to light mode
- Verify returns to light theme
- Screenshot of each

### T14: Tab Navigation
- Click Home tab → verify home page
- Click Decision Trees tab → verify DT content preserved
- Click Column Mapper tab → verify mapper state preserved
- Screenshot

---

## Test Execution Notes
- After each major action, take a screenshot for verification
- If any step fails, note the error and continue to next test
- Console errors should be checked after T3, T7, T9, T12
- Total estimated time: 10-15 minutes of browser interaction
