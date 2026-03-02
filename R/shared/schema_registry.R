# =============================================================================
# Schema Registry: Centralized Field Patterns and Domain Knowledge
# =============================================================================
#
# This module centralizes all field classification patterns, type detection
# logic, and domain thresholds that are currently scattered across app.R.
#
# All patterns are extracted from the actual implementation in:
# - ADF-CMS-Tool/app.R (lines 729-746, 1551-1556, 2683-2687, etc.)
# - See DATA_LAYER_KEY_BLOCKS.md for source locations
#
# Version: 1.0.0
# Phase: 1 (Extraction - No app.R changes yet)
# =============================================================================

#' Get default schema registry for MLS property data
#'
#' Returns a structured list of field patterns, type detection rules,
#' and quality thresholds used throughout the data pipeline.
#'
#' All patterns are derived from actual code in app.R (see line references).
#'
#' @param mls_format Optional: "generic" (default), or future format overrides
#' @return List with $patterns, $type_patterns, $thresholds
#' @export
#' @examples
#' schema <- get_default_schema()
#' schema$patterns$price
#' schema$thresholds$min_sample_size
get_default_schema <- function(mls_format = "generic") {

  schema <- list(

    # =========================================================================
    # FIELD ROLE PATTERNS
    # =========================================================================
    # These patterns classify columns into functional roles.
    # Source: app.R lines 729-746 (auto-exclusion), 1551-1556 (subject)

    patterns = list(

      # Long text / marketing content (EXCLUDE from modeling)
      # Source: app.R line 730
      marketing = c(
        "remarks",
        "comment",
        "description"
      ),

      # Specific address strings (EXCLUDE from modeling - high uniqueness)
      # Source: app.R line 731
      address = c(
        "^address$",
        "^street$",
        "^full.*address"
      ),

      # Technical identifiers (EXCLUDE from modeling)
      # Source: app.R line 732
      id = c(
        "^id$",
        "mls.?number",
        "apn",
        "parcel"
      ),

      # Metadata fields (EXCLUDE from modeling)
      # Source: app.R line 733
      metadata = c(
        "url",
        "link",
        "image",
        "photo",
        "^\\.mapper",        # .Mapper_Version, .Mapper_SHA, etc.
        "^mapper",           # Mapper metadata without dot prefix
        "version$",
        "sha$"
      ),

      # Geo coordinates (EXCLUDE from subject definition - too precise)
      # These are continuous values that don't make sense for subject matching
      geo_coordinates = c(
        "^longitude$",
        "^latitude$",
        "^lat$",
        "^lon$",
        "^lng$",
        "^long$",
        "coords",
        "coordinate"
      ),

      # Price and value fields (RESPONSE VARIABLE candidates)
      # Source: app.R lines 711, 756, 810, 2214
      price = c(
        "price",
        "value",
        "sold"
      ),

      # Date and time fields
      # Source: app.R lines 1551, 2207
      # Note: "sold", "list", "pend" can be dates OR prices (context-dependent)
      time = c(
        "date",
        "list",
        "pend",
        "sold"
      ),

      # Temporal/market timing variables (EXCLUDE per Charlie's methodology)
      # Source: app.R lines 1553, 2684
      temporal_market = c(
        "dom",
        "spap",
        "saleqtr"
      ),

      # Status and service fields (EXCLUDE from subject definition)
      # Source: app.R line 1552
      status_service = c(
        "status",
        "service",
        "terms"
      ),

      # Address components that ARE NOT addresses (keep for geo analysis)
      # These are NOT excluded - they're useful for modeling
      # Implicit from: "We explicitly KEEP City, Postal Code" (app.R line 727)
      geo_components = c(
        "city",
        "postalcode",
        "zip",
        "county"
      ),

      # Special case: Address-like price exclusions
      # Source: app.R line 2220 - Prevents "123 Sold Street" from being treated as price
      address_keywords = c(
        "street",
        "ave",
        "blvd",
        "road"
      ),

      # Listing status column patterns
      # Used to identify columns containing listing status information
      status = c(
        "^status$",
        "listing.*status",
        "mls.*status",
        "property.*status"
      ),

      # Canonical status values found in real dataset
      # These represent the actual status values observed in production data
      status_values = list(
        sold = "sold",
        active = "active",
        cancelled = "cancelled",
        expired = "expired"
      )
    ),

    # =========================================================================
    # TYPE DETECTION PATTERNS
    # =========================================================================
    # Logic for detecting if character columns should be numeric or factor.
    # Source: app.R lines 2187-2224 (validation function)

    type_patterns = list(

      # Characters to strip before checking if a column is numeric
      # Source: app.R line 2187
      numeric_strip_chars = "[$, ()]",

      # Regex to test if cleaned values are numeric
      # Source: app.R line 2192
      numeric_like_regex = "^-?[0-9.]+$",

      # Date column detection patterns
      # Source: app.R line 2207
      # If column name matches these AND doesn't match price patterns, it's a date
      date_like = c("date", "sold", "list", "pend"),
      date_like_exclusions = c("price", "value"),

      # Price column detection patterns
      # Source: app.R line 2214
      # These should ALWAYS be numeric (force conversion if needed)
      price_like = c("price", "value", "sold"),

      # Strings to recognize as NA during import
      # Source: app.R line 660
      na_strings = c("", "NA", "N/A", "null")
    ),

    # =========================================================================
    # QUALITY & CARDINALITY THRESHOLDS
    # =========================================================================
    # Hard-coded values extracted from validation and filtering logic.
    # Source: app.R lines 2148-2593 (validation function)

    thresholds = list(

      # Sample size requirements
      # Source: app.R lines 2148, 2341, 2467, 2478
      min_sample_size = 20,        # qeDT minsplit - absolute minimum
      warn_sample_size = 50,       # Warning threshold
      ideal_sample_size = 100,     # No warnings below this

      # Tree algorithm parameters (qeDT defaults)
      # Source: app.R lines 2148-2149
      minsplit = 20,
      minbucket = 7,

      # ID detection (character column with high uniqueness)
      # Source: app.R line 2167
      id_uniqueness_pct = 0.95,
      id_min_unique = 20,

      # Constant detection
      # Source: app.R line 2176
      constant_max_unique = 1,

      # High cardinality thresholds
      # Source: app.R lines 2251, 2320, 2531, 2553
      factor_reject_min_levels = 50,   # Too many levels - reject as factor
      factor_warn_min_levels = 20,     # Moderate cardinality - warn
      extreme_cardinality_pct = 0.80,  # Essentially a unique ID

      # Factor balance detection
      # Source: app.R lines 2275, 2502, 2519
      factor_imbalance_max_share = 0.95,  # Severely imbalanced
      factor_warn_max_share = 0.80,       # Moderately imbalanced

      # Missing data thresholds
      # Source: app.R lines 2408, 2425, 2450
      missing_high_pct = 20,      # Error level
      missing_moderate_pct = 5,   # Warning level
      missing_response_warn_pct = 10,  # Warning for response variable

      # Classification minimum requirements
      # Source: app.R line 2391
      min_class_observations = 10,
      ideal_class_observations = 30,

      # Outlier detection parameters (IQR method)
      # Uses Tukey's fences: Q1 - (multiplier × IQR), Q3 + (multiplier × IQR)
      # Common multipliers: 1.5 (standard), 3.0 (extreme only)
      outlier_iqr_multiplier = 3.0,    # Conservative: only remove extreme outliers
      outlier_remove_upper = TRUE,     # Remove high-priced outliers
      outlier_remove_lower = FALSE,    # Keep low-priced properties (may be valid distressed sales)
      price_column_name = "PriceSold"  # Column to apply outlier detection to
    ),

    # =========================================================================
    # MLS FORMAT OVERRIDES (Future)
    # =========================================================================
    # Placeholder for format-specific pattern variations
    mls_overrides = list()
  )

  # Apply format-specific overrides (future enhancement)
  if (mls_format != "generic") {
    warning("MLS format overrides not yet implemented. Using generic patterns.")
  }

  schema
}

# =============================================================================
# HELPER FUNCTIONS: Pattern Matching
# =============================================================================

#' Get field patterns for a specific role
#'
#' @param role One of: "marketing", "address", "id", "metadata", "price",
#'   "time", "temporal_market", "status_service", "geo_components"
#' @param schema Schema registry from get_default_schema()
#' @return Character vector of patterns
#' @export
get_field_pattern <- function(role, schema = get_default_schema()) {
  if (!role %in% names(schema$patterns)) {
    stop("Unknown role: '", role, "'. Available roles: ",
         paste(names(schema$patterns), collapse = ", "))
  }
  schema$patterns[[role]]
}

#' Check if a column name matches a pattern category
#'
#' @param column_name Name of column to check
#' @param role Pattern role to check against
#' @param schema Schema registry
#' @return Logical (TRUE if column matches pattern)
#' @export
#' @examples
#' is_field_type("PriceSold", "price")  # TRUE
#' is_field_type("Remarks", "marketing")  # TRUE
#' is_field_type("YearBuilt", "price")  # FALSE
is_field_type <- function(column_name, role, schema = get_default_schema()) {
  pattern <- get_field_pattern(role, schema)
  grepl(paste(pattern, collapse = "|"), column_name, ignore.case = TRUE)
}

#' Get auto-exclusion patterns for variable selection
#'
#' Returns the combined pattern used in app.R line 729-734 for auto-excluding
#' columns from analyst variable selection.
#'
#' @param schema Schema registry
#' @return Character vector combining marketing, address, id, metadata patterns
#' @export
get_auto_exclusion_patterns <- function(schema = get_default_schema()) {
  c(
    schema$patterns$marketing,
    schema$patterns$address,
    schema$patterns$id,
    schema$patterns$metadata
  )
}

#' Get subject exclusion patterns
#'
#' Returns the combined pattern used in app.R line 1551-1553 for excluding
#' columns from subject property definition.
#'
#' @param schema Schema registry
#' @return Character vector for subject column filtering
#' @export
get_subject_exclusion_patterns <- function(schema = get_default_schema()) {
  c(
    schema$patterns$time,
    schema$patterns$address,
    schema$patterns$marketing,
    schema$patterns$id,
    schema$patterns$status_service,
    schema$patterns$price,
    schema$patterns$temporal_market,
    schema$patterns$metadata,         # Exclude mapper metadata, URLs, etc.
    schema$patterns$geo_coordinates   # Exclude lat/lon (too precise for subject)
  )
}

#' Get temporal/market exclusion patterns
#'
#' Returns patterns for DOM, SPAP, SaleQtr used in app.R line 2684.
#'
#' @param schema Schema registry
#' @return Character vector of temporal market patterns
#' @export
get_temporal_exclusion_patterns <- function(schema = get_default_schema()) {
  schema$patterns$temporal_market
}

# =============================================================================
# HELPER FUNCTIONS: Response Variable Detection
# =============================================================================

#' Detect likely response variable (price column)
#'
#' Replicates logic from app.R lines 710-716, 756, 810.
#' Looks for numeric columns matching price patterns.
#'
#' @param data Input data frame
#' @param schema Schema registry
#' @return Name of most likely response variable, or NA if not found
#' @export
#' @examples
#' data <- data.frame(PriceSold = c(100000, 200000), Beds = c(3, 4))
#' detect_response_variable(data)  # "PriceSold"
detect_response_variable <- function(data, schema = get_default_schema()) {

  all_cols <- names(data)
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  # Build price pattern regex
  price_pattern <- paste(schema$patterns$price, collapse = "|")

  # First: Look for numeric columns matching price patterns
  likely_price <- numeric_cols[grepl(price_pattern, numeric_cols, ignore.case = TRUE)][1]

  # Fallback: Look for ANY column matching price patterns (even if not numeric)
  if (is.na(likely_price)) {
    likely_price <- all_cols[grepl(price_pattern, all_cols, ignore.case = TRUE)][1]
  }

  likely_price
}

# =============================================================================
# HELPER FUNCTIONS: Type Detection
# =============================================================================

#' Check if column name suggests it's a date
#'
#' Replicates logic from app.R line 2207.
#' Returns TRUE if column name matches date patterns but NOT price patterns.
#'
#' @param column_name Name of column
#' @param schema Schema registry
#' @return Logical
#' @export
is_date_column <- function(column_name, schema = get_default_schema()) {

  date_pattern <- paste(schema$type_patterns$date_like, collapse = "|")
  price_pattern <- paste(schema$type_patterns$date_like_exclusions, collapse = "|")

  matches_date <- grepl(date_pattern, column_name, ignore.case = TRUE)
  matches_price <- grepl(price_pattern, column_name, ignore.case = TRUE)

  matches_date && !matches_price
}

#' Check if column name suggests it's a price
#'
#' Replicates logic from app.R line 2214.
#' Returns TRUE if column name matches price patterns and is NOT a date.
#'
#' @param column_name Name of column
#' @param schema Schema registry
#' @return Logical
#' @export
is_price_column <- function(column_name, schema = get_default_schema()) {

  price_pattern <- paste(schema$type_patterns$price_like, collapse = "|")
  is_date <- is_date_column(column_name, schema)

  grepl(price_pattern, column_name, ignore.case = TRUE) && !is_date
}

#' Check if price column name contains address keywords
#'
#' Replicates logic from app.R line 2220.
#' Used to prevent "123 Sold Street" from being treated as a price column.
#'
#' @param column_name Name of column
#' @param schema Schema registry
#' @return Logical (TRUE if contains address keywords)
#' @export
contains_address_keywords <- function(column_name, schema = get_default_schema()) {
  address_pattern <- paste(schema$patterns$address_keywords, collapse = "|")
  grepl(address_pattern, column_name, ignore.case = TRUE)
}

# =============================================================================
# HELPER FUNCTIONS: Status Classification
# =============================================================================

#' Classify listing status values into roles
#'
#' Maps raw status strings from MLS data into standardized categories.
#' Based on real dataset with values: "Sold", "Active", "Cancelled", "Expired".
#'
#' @param status_vector Character vector (e.g., data$Status)
#' @return Factor with levels: "sold", "unsold", "active", "unknown"
#' @export
#' @examples
#' status <- c("Sold", "Active", "Cancelled", "Expired", "SOLD", NA, "Other")
#' classify_listing_status(status)
#' # Returns: factor with "sold", "active", "unsold", "unsold", "sold", "unknown", "unknown"
classify_listing_status <- function(status_vector) {

  # Normalize: lowercase and trim whitespace
  x <- tolower(trimws(as.character(status_vector)))

  # Handle NA/missing values
  x[is.na(x)] <- ""

  # Classify into roles
  sold    <- x == "sold"
  active  <- x == "active"
  unsold  <- x %in% c("cancelled", "expired")

  # Map to standardized categories
  out <- ifelse(sold,   "sold",
         ifelse(active, "active",
         ifelse(unsold, "unsold",
                "unknown")))

  # Return as factor with explicit levels
  factor(out, levels = c("sold", "unsold", "active", "unknown"))
}

# =============================================================================
# HELPER FUNCTIONS: Threshold Accessors
# =============================================================================

#' Get quality threshold by name
#'
#' @param threshold_name Name of threshold (e.g., "min_sample_size")
#' @param schema Schema registry
#' @return Numeric threshold value
#' @export
get_threshold <- function(threshold_name, schema = get_default_schema()) {
  if (!threshold_name %in% names(schema$thresholds)) {
    stop("Unknown threshold: '", threshold_name, "'. Available: ",
         paste(names(schema$thresholds), collapse = ", "))
  }
  schema$thresholds[[threshold_name]]
}

#' Get all thresholds as a named list
#'
#' @param schema Schema registry
#' @return Named list of all threshold values
#' @export
get_all_thresholds <- function(schema = get_default_schema()) {
  schema$thresholds
}

# =============================================================================
# HELPER FUNCTIONS: Pattern Application
# =============================================================================

#' Apply auto-exclusion logic to column names
#'
#' Replicates app.R lines 736-739.
#' Returns columns that should be auto-selected (excluded = FALSE).
#'
#' @param column_names Character vector of all column names
#' @param schema Schema registry
#' @return Character vector of columns to include (auto-selected)
#' @export
#' @examples
#' cols <- c("PriceSold", "Beds", "Remarks", "MLSNumber", "City")
#' apply_auto_exclusion(cols)
#' # Returns: "PriceSold", "Beds", "City"
apply_auto_exclusion <- function(column_names, schema = get_default_schema()) {

  exclusion_patterns <- get_auto_exclusion_patterns(schema)
  pattern_regex <- paste(exclusion_patterns, collapse = "|")

  # Identify columns to exclude
  auto_exclude <- grepl(pattern_regex, column_names, ignore.case = TRUE)

  # Return columns NOT excluded
  column_names[!auto_exclude]
}

#' Apply subject exclusion logic to column names
#'
#' Replicates app.R lines 1555-1556.
#' Returns columns suitable for subject property definition.
#'
#' @param column_names Character vector of all column names
#' @param schema Schema registry
#' @return Character vector of columns suitable for subject
#' @export
apply_subject_exclusion <- function(column_names, schema = get_default_schema()) {

  exclusion_patterns <- get_subject_exclusion_patterns(schema)
  pattern_regex <- paste(exclusion_patterns, collapse = "|")

  # Identify columns to exclude
  exclude <- grepl(pattern_regex, column_names, ignore.case = TRUE)

  # Return columns NOT excluded
  column_names[!exclude]
}

#' Apply temporal exclusion logic to column names
#'
#' Replicates app.R lines 2685-2686.
#' Returns columns with temporal/market variables removed.
#'
#' @param column_names Character vector of column names
#' @param schema Schema registry
#' @return Character vector with temporal columns removed
#' @export
apply_temporal_exclusion <- function(column_names, schema = get_default_schema()) {

  temporal_patterns <- get_temporal_exclusion_patterns(schema)
  pattern_regex <- paste(temporal_patterns, collapse = "|")

  # Identify temporal columns
  is_temporal <- grepl(pattern_regex, column_names, ignore.case = TRUE)

  # Return columns that are NOT temporal
  column_names[!is_temporal]
}

# =============================================================================
# VALIDATION HELPERS
# =============================================================================

#' Validate that a schema registry is well-formed
#'
#' Checks that all expected components are present.
#'
#' @param schema Schema registry to validate
#' @return Logical (TRUE if valid, stops with error otherwise)
#' @export
validate_schema <- function(schema) {

  # Check top-level structure
  required_components <- c("patterns", "type_patterns", "thresholds", "mls_overrides")
  missing <- setdiff(required_components, names(schema))
  if (length(missing) > 0) {
    stop("Schema missing required components: ", paste(missing, collapse = ", "))
  }

  # Check patterns
  required_patterns <- c("marketing", "address", "id", "metadata", "price",
                        "time", "temporal_market", "status_service",
                        "geo_components", "geo_coordinates", "address_keywords", "status", "status_values")
  missing_patterns <- setdiff(required_patterns, names(schema$patterns))
  if (length(missing_patterns) > 0) {
    stop("Schema patterns missing: ", paste(missing_patterns, collapse = ", "))
  }

  # Check type patterns
  required_type_patterns <- c("numeric_strip_chars", "numeric_like_regex",
                             "date_like", "date_like_exclusions",
                             "price_like", "na_strings")
  missing_type <- setdiff(required_type_patterns, names(schema$type_patterns))
  if (length(missing_type) > 0) {
    stop("Schema type_patterns missing: ", paste(missing_type, collapse = ", "))
  }

  # Check thresholds
  required_thresholds <- c("min_sample_size", "minsplit", "minbucket",
                          "id_uniqueness_pct", "constant_max_unique",
                          "factor_reject_min_levels", "missing_high_pct")
  missing_thresh <- setdiff(required_thresholds, names(schema$thresholds))
  if (length(missing_thresh) > 0) {
    stop("Schema thresholds missing: ", paste(missing_thresh, collapse = ", "))
  }

  TRUE
}

# =============================================================================
# PACKAGE DOCUMENTATION
# =============================================================================

#' Schema Registry for MLS Property Data
#'
#' This module provides centralized field patterns, type detection rules,
#' and quality thresholds for the CA-ADF-CMS data pipeline.
#'
#' @section Main Function:
#' \code{\link{get_default_schema}} - Returns the complete schema registry
#'
#' @section Pattern Helpers:
#' \itemize{
#'   \item \code{\link{get_field_pattern}} - Get patterns for a role
#'   \item \code{\link{is_field_type}} - Test if column matches a role
#'   \item \code{\link{apply_auto_exclusion}} - Apply variable selection logic
#'   \item \code{\link{apply_subject_exclusion}} - Apply subject filtering logic
#'   \item \code{\link{apply_temporal_exclusion}} - Remove temporal variables
#' }
#'
#' @section Type Detection:
#' \itemize{
#'   \item \code{\link{detect_response_variable}} - Find price column
#'   \item \code{\link{is_date_column}} - Test if column is a date
#'   \item \code{\link{is_price_column}} - Test if column is a price
#' }
#'
#' @section Threshold Accessors:
#' \itemize{
#'   \item \code{\link{get_threshold}} - Get a threshold value
#'   \item \code{\link{get_all_thresholds}} - Get all thresholds
#' }
#'
#' @docType package
#' @name schema_registry
NULL
