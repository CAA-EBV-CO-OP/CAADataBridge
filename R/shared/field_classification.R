# =============================================================================
# Field Classification Module: Assign Roles and Relevance to Variables
# =============================================================================
#
# This module classifies data columns into functional roles (id, geo, physical,
# price, etc.) and assigns relevance levels for tree modeling (core, extended,
# ignore).
#
# Based on real dataset columns from CA-ADF-CMS application.
#
# Version: 1.0.0
# Phase: 2 (Field Classification - Pure Functions)
# =============================================================================

# Required packages
# - dplyr (case_when, tibble)

# Source schema registry
if (!exists("get_default_schema")) {
  source("R/shared/schema_registry.R")
}

#' Classify variables into roles and relevance for tree modeling
#'
#' Analyzes each column in the dataset and assigns:
#' - Role: functional category (id, geo, physical, price, time, etc.)
#' - Relevance: importance for tree modeling (core, extended, ignore)
#' - Reason: explanation for classification
#'
#' Uses patterns from schema registry combined with domain knowledge
#' about specific MLS fields.
#'
#' @param data Data frame or tibble (e.g., from import_adf_file()$adf)
#' @param response_var Name of response variable (e.g., "Sold Price").
#'   If provided, this column gets relevance "core" and role "price".
#' @param schema Schema registry from get_default_schema()
#' @return Data frame with columns:
#'   \describe{
#'     \item{name}{Character: column name}
#'     \item{role}{Character: functional role (id, address, geo, physical, time,
#'                 price, status, text_long, metadata, other)}
#'     \item{relevance}{Character: importance level (core, extended, ignore)}
#'     \item{reason}{Character: explanation for classification}
#'   }
#' @export
#' @examples
#' \dontrun{
#' result <- import_adf_file("data.csv")
#' classification <- classify_variables_for_tree(
#'   result$adf,
#'   response_var = result$response_var
#' )
#' View(classification)
#' }
classify_variables_for_tree <- function(
  data,
  response_var = NULL,
  schema = get_default_schema()
) {

  all_cols <- names(data)

  # Initialize results
  classifications <- lapply(all_cols, function(col_name) {

    # Start with default classification
    role <- "other"
    relevance <- "extended"
    reason <- ""

    # =========================================================================
    # STEP 1: PATTERN-BASED CLASSIFICATION (Schema Registry)
    # =========================================================================

    # ID fields
    if (is_field_type(col_name, "id", schema)) {
      role <- "id"
      reason <- "Technical identifier (excluded from modeling)"
    }

    # Metadata
    else if (is_field_type(col_name, "metadata", schema)) {
      role <- "metadata"
      reason <- "Metadata field (URLs, photos - excluded)"
    }

    # Marketing/Remarks (long text)
    else if (is_field_type(col_name, "marketing", schema)) {
      role <- "text_long"
      reason <- "Long text / marketing content (excluded)"
    }

    # Address
    else if (is_field_type(col_name, "address", schema)) {
      role <- "address"
      reason <- "Address string (high uniqueness - excluded)"
    }

    # Price
    else if (is_field_type(col_name, "price", schema)) {
      role <- "price"
      reason <- "Price or value field"
    }

    # Time/Date
    else if (is_field_type(col_name, "time", schema)) {
      role <- "time"
      reason <- "Date or time field"
    }

    # Temporal/Market (DOM, SPAP, SaleQtr)
    else if (is_field_type(col_name, "temporal_market", schema)) {
      role <- "time"
      reason <- "Temporal/market timing variable (Charlie's exclusion)"
    }

    # Status
    else if (is_field_type(col_name, "status", schema)) {
      role <- "status"
      reason <- "Listing status field"
    }

    # Geo components (City, Zip)
    else if (is_field_type(col_name, "geo_components", schema)) {
      role <- "geo"
      reason <- "Geographic location component"
    }

    # =========================================================================
    # STEP 2: SPECIFIC COLUMN OVERRIDES (Real Dataset)
    # =========================================================================
    # These handle exact column names from the CA-ADF dataset

    # --- IDs ---
    if (col_name %in% c("MLS #", "Listing Key Numeric", "Parcel ID")) {
      role <- "id"
      reason <- "Unique listing identifier (excluded)"
    }

    # --- Address & Legal Text ---
    else if (col_name == "Address") {
      role <- "address"
      reason <- "Property address string (excluded)"
    }

    else if (col_name == "Tax Legal Description") {
      role <- "text_long"
      reason <- "Legal description text (excluded)"
    }

    else if (col_name %in% c("Public Remarks", "REALTOR® Remarks")) {
      role <- "text_long"
      reason <- "Marketing remarks / long text (excluded)"
    }

    # --- Status ---
    else if (col_name == "Status") {
      role <- "status"
      reason <- "Listing status (sold/active/cancelled/expired)"
    }

    # --- Dates ---
    else if (col_name %in% c("On Market Date", "Off Market Date", "Sold Date",
                             "Purchase Contract Date", "Title/Ownership Change Date")) {
      role <- "time"
      reason <- "Date field"
    }

    # --- Price Fields ---
    else if (col_name == "Sold Price") {
      role <- "price"
      reason <- "Sale price (primary response variable)"
    }

    else if (col_name %in% c("List Price", "Original List Price")) {
      role <- "price"
      reason <- "List price (predictor or alternate response)"
    }

    # --- Physical Characteristics (Core Features) ---
    else if (col_name %in% c(
      "Property Sub Type", "Year Built", "Architectural Style",
      "Bedrooms Total", "Bathrooms Full", "Bathrooms Half", "Ensuite Bathrooms",
      "Total Kitchens", "Levels",
      "Total Finished SqFt", "Above Grade Finished Sqft", "Below Grade Finished SqFt",
      "Basement YN", "Basement",
      "Garage Spaces", "Garage YN",
      "Pool YN",
      "Lot Acres", "Lot Size Dimensions L x W",
      "Construction Materials"
    )) {
      role <- "physical"
      reason <- "Physical property characteristic"
    }

    # --- Geographic / Location (Core) ---
    else if (col_name %in% c(
      "City", "Postal Code",
      "Major Area / Region", "Minor / Sub Area",
      "Latitude", "Longitude",
      "Zoning"
    )) {
      role <- "geo"
      reason <- "Geographic location or zoning"
    }

    # --- Waterfront & Utilities (Extended Features) ---
    else if (col_name %in% c(
      "Waterfront YN", "Waterfrontage", "Waterfront Features",
      "Sewer", "Water Source"
    )) {
      role <- "physical"
      reason <- "Utility or waterfront feature"
    }

    # --- Special Listing Conditions ---
    else if (col_name == "Special Listing Conditions") {
      role <- "metadata"
      reason <- "Special conditions metadata"
    }

    # =========================================================================
    # STEP 3: ASSIGN RELEVANCE BASED ON ROLE
    # =========================================================================

    relevance <- default_relevance_for_role(role)

    # --- OVERRIDE: Response variable is always "core" ---
    if (!is.null(response_var) && col_name == response_var) {
      relevance <- "core"
      reason <- paste0(reason, " (RESPONSE VARIABLE)")
    }

    # --- OVERRIDE: List Price is "extended" even though role is "price" ---
    if (col_name %in% c("List Price", "Original List Price")) {
      relevance <- "extended"
    }

    # --- OVERRIDE: Status is "extended" for primary valuation tree ---
    if (role == "status") {
      relevance <- "extended"
    }

    # --- OVERRIDE: Waterfront/utilities are "extended" ---
    if (col_name %in% c("Waterfront YN", "Waterfrontage", "Waterfront Features",
                        "Sewer", "Water Source")) {
      relevance <- "extended"
    }

    # --- OVERRIDE: Lot Size Dimensions is text, not core ---
    if (col_name == "Lot Size Dimensions L x W") {
      relevance <- "extended"
      reason <- "Lot dimensions (text format - use Lot Acres instead)"
    }

    # =========================================================================
    # STEP 4: RETURN CLASSIFICATION
    # =========================================================================

    list(
      name = col_name,
      role = role,
      relevance = relevance,
      reason = reason
    )
  })

  # Convert to data frame
  dplyr::bind_rows(classifications)
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get default relevance level for a role
#'
#' Internal helper that maps roles to default relevance levels.
#'
#' @param role Character: functional role (id, geo, physical, etc.)
#' @return Character: "core", "extended", or "ignore"
#' @keywords internal
default_relevance_for_role <- function(role) {
  dplyr::case_when(
    role %in% c("price", "physical", "geo") ~ "core",
    role %in% c("time", "status", "other") ~ "extended",
    role %in% c("id", "address", "text_long", "metadata") ~ "ignore",
    TRUE ~ "extended"
  )
}

#' Get columns by role
#'
#' Filter classification results to get columns with a specific role.
#'
#' @param classification Output from classify_variables_for_tree()
#' @param role Character: role to filter by (e.g., "physical", "geo")
#' @return Character vector of column names
#' @export
get_columns_by_role <- function(classification, role) {
  classification$name[classification$role == role]
}

#' Get columns by relevance
#'
#' Filter classification results to get columns with a specific relevance.
#'
#' @param classification Output from classify_variables_for_tree()
#' @param relevance Character: relevance level ("core", "extended", "ignore")
#' @return Character vector of column names
#' @export
get_columns_by_relevance <- function(classification, relevance) {
  classification$name[classification$relevance == relevance]
}

#' Get core modeling variables
#'
#' Returns columns marked as "core" relevance, suitable for primary
#' valuation tree modeling.
#'
#' @param classification Output from classify_variables_for_tree()
#' @param exclude_response Logical: if TRUE, exclude response variable
#' @param response_var Character: name of response variable to exclude
#' @return Character vector of core predictor column names
#' @export
get_core_predictors <- function(classification, exclude_response = TRUE, response_var = NULL) {

  core_cols <- get_columns_by_relevance(classification, "core")

  if (exclude_response && !is.null(response_var)) {
    core_cols <- setdiff(core_cols, response_var)
  }

  core_cols
}

#' Get extended modeling variables
#'
#' Returns columns marked as "extended" relevance.
#'
#' @param classification Output from classify_variables_for_tree()
#' @return Character vector of extended column names
#' @export
get_extended_predictors <- function(classification) {
  get_columns_by_relevance(classification, "extended")
}

#' Get ignored variables
#'
#' Returns columns marked as "ignore" relevance (IDs, addresses, long text).
#'
#' @param classification Output from classify_variables_for_tree()
#' @return Character vector of ignored column names
#' @export
get_ignored_variables <- function(classification) {
  get_columns_by_relevance(classification, "ignore")
}

#' Summarize classification results
#'
#' Generate summary statistics for variable classification.
#'
#' @param classification Output from classify_variables_for_tree()
#' @return Named list with counts by role and relevance
#' @export
summarize_classification <- function(classification) {

  list(
    total_vars = nrow(classification),
    by_role = table(classification$role),
    by_relevance = table(classification$relevance),
    core_count = sum(classification$relevance == "core"),
    extended_count = sum(classification$relevance == "extended"),
    ignored_count = sum(classification$relevance == "ignore")
  )
}

#' Print classification summary
#'
#' Pretty-print classification results to console.
#'
#' @param classification Output from classify_variables_for_tree()
#' @export
print_classification_summary <- function(classification) {

  summary <- summarize_classification(classification)

  cat("\n=== Variable Classification Summary ===\n\n")
  cat(sprintf("Total Variables: %d\n\n", summary$total_vars))

  cat("By Relevance:\n")
  cat(sprintf("  Core:     %d (primary modeling variables)\n", summary$core_count))
  cat(sprintf("  Extended: %d (analyst choice / secondary)\n", summary$extended_count))
  cat(sprintf("  Ignore:   %d (excluded from modeling)\n", summary$ignored_count))

  cat("\nBy Role:\n")
  role_counts <- summary$by_role
  for (role_name in names(role_counts)) {
    cat(sprintf("  %-12s: %d\n", role_name, role_counts[role_name]))
  }

  cat("\n")
  invisible(summary)
}

#' Print detailed classification table
#'
#' Display full classification results with formatting.
#'
#' @param classification Output from classify_variables_for_tree()
#' @param relevance_filter Optional: filter to specific relevance ("core", "extended", "ignore")
#' @export
print_classification_table <- function(classification, relevance_filter = NULL) {

  if (!is.null(relevance_filter)) {
    classification <- classification[classification$relevance == relevance_filter, ]
    cat(sprintf("\n=== Variables: %s ===\n\n", toupper(relevance_filter)))
  } else {
    cat("\n=== Variable Classification ===\n\n")
  }

  if (nrow(classification) == 0) {
    cat("No variables in this category.\n\n")
    return(invisible(NULL))
  }

  # Print in a readable format
  for (i in 1:nrow(classification)) {
    row <- classification[i, ]
    cat(sprintf("%-35s  [%s]  %s\n",
                row$name,
                toupper(row$relevance),
                row$role))
    cat(sprintf("    %s\n", row$reason))
    if (i < nrow(classification)) cat("\n")
  }

  cat("\n")
  invisible(classification)
}

#' Export classification to CSV
#'
#' Save classification results to a CSV file for review.
#'
#' @param classification Output from classify_variables_for_tree()
#' @param path File path for output CSV
#' @export
export_classification <- function(classification, path) {
  write.csv(classification, path, row.names = FALSE)
  message("Classification exported to: ", path)
  invisible(path)
}

# =============================================================================
# VALIDATION HELPERS
# =============================================================================

#' Validate classification result structure
#'
#' Checks that classify_variables_for_tree() returned expected format.
#'
#' @param classification Output from classify_variables_for_tree()
#' @return Logical (TRUE if valid, stops with error otherwise)
#' @export
validate_classification <- function(classification) {

  # Check it's a data frame
  if (!is.data.frame(classification)) {
    stop("Classification must be a data.frame")
  }

  # Check required columns
  required_cols <- c("name", "role", "relevance", "reason")
  missing <- setdiff(required_cols, names(classification))
  if (length(missing) > 0) {
    stop("Classification missing columns: ", paste(missing, collapse = ", "))
  }

  # Check allowed values
  allowed_roles <- c("id", "address", "geo", "physical", "time", "price",
                    "status", "text_long", "metadata", "other")
  invalid_roles <- setdiff(unique(classification$role), allowed_roles)
  if (length(invalid_roles) > 0) {
    warning("Unexpected roles found: ", paste(invalid_roles, collapse = ", "))
  }

  allowed_relevance <- c("core", "extended", "ignore")
  invalid_relevance <- setdiff(unique(classification$relevance), allowed_relevance)
  if (length(invalid_relevance) > 0) {
    stop("Invalid relevance values: ", paste(invalid_relevance, collapse = ", "))
  }

  TRUE
}

# =============================================================================
# PACKAGE DOCUMENTATION
# =============================================================================

#' Field Classification Module
#'
#' Pure functions for classifying MLS data columns into roles and relevance
#' levels for tree modeling.
#'
#' @section Main Function:
#' \code{\link{classify_variables_for_tree}} - Classify all variables
#'
#' @section Helper Functions:
#' \itemize{
#'   \item \code{\link{get_columns_by_role}} - Filter by role
#'   \item \code{\link{get_columns_by_relevance}} - Filter by relevance
#'   \item \code{\link{get_core_predictors}} - Get core modeling vars
#'   \item \code{\link{get_extended_predictors}} - Get extended vars
#'   \item \code{\link{get_ignored_variables}} - Get excluded vars
#' }
#'
#' @section Summary Functions:
#' \itemize{
#'   \item \code{\link{summarize_classification}} - Get summary stats
#'   \item \code{\link{print_classification_summary}} - Print summary
#'   \item \code{\link{print_classification_table}} - Print detailed table
#'   \item \code{\link{export_classification}} - Export to CSV
#' }
#'
#' @section Usage:
#' \preformatted{
#' # Basic usage
#' result <- import_adf_file("data.csv")
#' classification <- classify_variables_for_tree(
#'   result$adf,
#'   response_var = result$response_var
#' )
#'
#' # View summary
#' print_classification_summary(classification)
#'
#' # Get core predictors
#' core_vars <- get_core_predictors(classification,
#'                                  exclude_response = TRUE,
#'                                  response_var = result$response_var)
#'
#' # View detailed table by relevance
#' print_classification_table(classification, relevance_filter = "core")
#' print_classification_table(classification, relevance_filter = "ignore")
#' }
#'
#' @docType package
#' @name field_classification
NULL
