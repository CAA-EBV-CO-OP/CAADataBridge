# Column Mapper - A Shiny app for mapping CSV columns to standardized field names
# License: MIT
# Repository: https://github.com/PaulERayburn/CValR_4.0
#
# ============================================================================
# CSV EXPORT STANDARD
# ============================================================================
# This application uses RFC 4180 compliant CSV export via readr::write_csv()
# to ensure maximum compatibility with downstream tools and modern CSV parsers.
#
# Why RFC 4180?
# - Proper handling of quoted fields containing commas, quotes, and newlines
# - Consistent behavior across readr::read_csv(), data.table::fread(), and
#   other modern parsers (Python pandas, etc.)
# - Prevents "Ignoring observations" warnings and row loss in downstream tools
# - Resolves incompatibility between base R write.csv() and readr::read_csv()
#
# Implementation: export_csv_robust() helper function (see below)
# ============================================================================

library(shiny)
options(shiny.maxRequestSize = 50 * 1024^2)  # 50 MB upload limit
library(shinyWidgets)
library(shinybusy)  # Progress spinners for long-running operations
library(DT)
library(jsonlite)
library(ggplot2)
library(scales)
library(readxl)  # used for Excel uploads
library(readr)   # used for robust CSV I/O (RFC 4180 compliant)
library(mongolite)  # MongoDB for user profiles and persistent storage
library(digest)     # Password hashing
library(shinyjs)    # JavaScript operations for UI show/hide

# ============================================================================
# PYTHON/RAPIDFUZZ INTEGRATION FOR OPTIMIZED STRING MATCHING
# ============================================================================
# Uses reticulate to call rapidfuzz (C++ optimized) for faster fuzzy matching.
# Falls back to pure R if Python/rapidfuzz not available.
# Install: pip install rapidfuzz
# ============================================================================

# Initialize Python availability flag
RAPIDFUZZ_AVAILABLE <- FALSE

# Try to load reticulate and rapidfuzz
tryCatch({
  if (requireNamespace("reticulate", quietly = TRUE)) {
    library(reticulate)

    # Check if rapidfuzz is available
    if (py_module_available("rapidfuzz")) {
      rapidfuzz <- import("rapidfuzz")
      rapidfuzz_fuzz <- rapidfuzz$fuzz
      rapidfuzz_process <- rapidfuzz$process
      RAPIDFUZZ_AVAILABLE <- TRUE
      message("[rapidfuzz] Loaded - optimized string matching enabled")
    } else {
      message("[rapidfuzz] Not installed - using R fallback (pip install rapidfuzz for faster matching)")
    }
  } else {
    message("[reticulate] Not installed - using R fallback")
  }
}, error = function(e) {
  message("[rapidfuzz] Setup error: ", e$message, " - using R fallback")
})

#' Batch fuzzy match using rapidfuzz (Python)
#'
#' @param query Character string to match against choices
#' @param choices Character vector of possible matches
#' @param score_cutoff Minimum score (0-100) to include in results
#' @return Data frame with columns: choice, score, index (1-based) or NULL if unavailable
batch_fuzzy_match <- function(query, choices, score_cutoff = 60) {
  if (!RAPIDFUZZ_AVAILABLE || length(choices) == 0) {
    return(NULL)  # Signal to use R fallback

  }

  tryCatch({
    # rapidfuzz.process.extract returns list of (match, score, index)
    results <- rapidfuzz_process$extract(
      query,
      choices,
      scorer = rapidfuzz_fuzz$token_set_ratio,
      score_cutoff = as.integer(score_cutoff),
      limit = NULL  # Return all matches above cutoff
    )

    # Convert to R-friendly format
    if (length(results) == 0) return(data.frame(choice = character(), score = numeric(), index = integer()))

    data.frame(
      choice = sapply(results, function(x) x[[1]]),
      score = sapply(results, function(x) x[[2]]),
      index = sapply(results, function(x) x[[3]]) + 1L  # Python 0-indexed to R 1-indexed
    )
  }, error = function(e) {
    warning("[rapidfuzz] Error during matching: ", e$message)
    NULL
  })
}

# ============================================================================
# MONGODB CONNECTION & USER MANAGEMENT
# ============================================================================
# MongoDB Atlas connection for user authentication and profile persistence.
# Connection string should be set via environment variable MONGODB_URI
# or in a .Renviron file (not committed to git).
#
# Collections:
#   - users: User accounts (username, email, password_hash, created_at)
#   - profiles: Saved mapping profiles (user_id, profile_name, profile_data, updated_at)
# ============================================================================

# Get MongoDB connection string from environment
get_mongodb_uri <- function() {

  uri <- Sys.getenv("MONGODB_URI", "")
  if (!nzchar(uri)) {
    # Fallback: try to read from .Renviron in app directory
    renviron_path <- file.path(getwd(), ".Renviron")
    if (file.exists(renviron_path)) {
      readRenviron(renviron_path)
      uri <- Sys.getenv("MONGODB_URI", "")
    }
  }
  uri
}

# Create MongoDB connection to a collection
# Returns NULL if connection fails (allows app to run without MongoDB)
mongo_connect <- function(collection, db = "caa_clearvalr") {
  uri <- get_mongodb_uri()
  if (!nzchar(uri)) {
    message("MongoDB URI not configured - running in local mode")
    return(NULL)
  }
  tryCatch({
    mongolite::mongo(collection = collection, db = db, url = uri)
  }, error = function(e) {
    message("MongoDB connection failed: ", e$message)
    NULL
  })
}

# Hash password using SHA-256 with salt
hash_password <- function(password, salt = NULL) {
  if (is.null(salt)) {
    salt <- paste0(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
  }
  hash <- digest::digest(paste0(salt, password), algo = "sha256")
  paste0(salt, ":", hash)
}

# Verify password against stored hash
verify_password <- function(password, stored_hash) {
  parts <- strsplit(stored_hash, ":", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(FALSE)
  salt <- parts[1]
  expected <- hash_password(password, salt)
  identical(expected, stored_hash)
}

# ============================================================================
# USER AUTHENTICATION FUNCTIONS
# ============================================================================

#' Create a new user account
#' @param username Unique username
#' @param email User email
#' @param password Plain text password (will be hashed)
#' @return List with success status and message
create_user <- function(username, email, password) {
  users <- mongo_connect("users")
  if (is.null(users)) {
    return(list(success = FALSE, message = "Database not available"))
  }
  on.exit(users$disconnect(), add = TRUE)

  # Check if username already exists
  existing <- users$find(sprintf('{"username": "%s"}', username))
  if (nrow(existing) > 0) {
    return(list(success = FALSE, message = "Username already exists"))
  }

  # Check if email already exists
  existing_email <- users$find(sprintf('{"email": "%s"}', tolower(email)))
  if (nrow(existing_email) > 0) {
    return(list(success = FALSE, message = "Email already registered"))
  }

  # Create user document
  user_doc <- data.frame(
    username = username,
    email = tolower(email),
    password_hash = hash_password(password),
    created_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  tryCatch({
    users$insert(user_doc)
    list(success = TRUE, message = "Account created successfully")
  }, error = function(e) {
    list(success = FALSE, message = paste("Error creating account:", e$message))
  })
}

#' Authenticate a user
#' @param username Username
#' @param password Plain text password
#' @return List with success status, message, and user_id if successful
authenticate_user <- function(username, password) {
  users <- mongo_connect("users")
  if (is.null(users)) {
    return(list(success = FALSE, message = "Database not available"))
  }
  on.exit(users$disconnect(), add = TRUE)

  user <- users$find(sprintf('{"username": "%s"}', username))
  if (nrow(user) == 0) {
    return(list(success = FALSE, message = "Invalid username or password"))
  }

  if (!verify_password(password, user$password_hash[1])) {
    return(list(success = FALSE, message = "Invalid username or password"))
  }

  list(
    success = TRUE,
    message = "Login successful",
    user_id = user$`_id`[1],
    username = user$username[1],
    email = user$email[1]
  )
}

# ============================================================================
# PROFILE STORAGE FUNCTIONS
# ============================================================================

#' Save a mapping profile to MongoDB
#' @param user_id User's MongoDB ID
#' @param profile_name Name for the profile
#' @param profile_data List containing the profile data (mappings, exclusions, etc.)
#' @return List with success status and message
save_profile_to_db <- function(user_id, profile_name, profile_data) {
  profiles <- mongo_connect("profiles")
  if (is.null(profiles)) {
    return(list(success = FALSE, message = "Database not available"))
  }
  on.exit(profiles$disconnect(), add = TRUE)

  # Convert profile_data to JSON string for storage
  profile_json <- jsonlite::toJSON(profile_data, auto_unbox = TRUE)

  # Check if profile with same name exists for this user
  existing <- profiles$find(sprintf(
    '{"user_id": "%s", "profile_name": "%s"}',
    user_id, profile_name
  ))

  if (nrow(existing) > 0) {
    # Update existing profile
    profiles$update(
      sprintf('{"user_id": "%s", "profile_name": "%s"}', user_id, profile_name),
      sprintf('{"$set": {"profile_data": %s, "updated_at": "%s"}}',
              profile_json, as.character(Sys.time()))
    )
    return(list(success = TRUE, message = "Profile updated"))
  } else {
    # Insert new profile
    profile_doc <- data.frame(
      user_id = user_id,
      profile_name = profile_name,
      profile_data = as.character(profile_json),
      created_at = as.character(Sys.time()),
      updated_at = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    profiles$insert(profile_doc)
    return(list(success = TRUE, message = "Profile saved"))
  }
}

#' Load a mapping profile from MongoDB
#' @param user_id User's MongoDB ID
#' @param profile_name Name of the profile to load
#' @return List with success status and profile_data if successful
load_profile_from_db <- function(user_id, profile_name) {
  profiles <- mongo_connect("profiles")
  if (is.null(profiles)) {
    return(list(success = FALSE, message = "Database not available"))
  }
  on.exit(profiles$disconnect(), add = TRUE)

  profile <- profiles$find(sprintf(
    '{"user_id": "%s", "profile_name": "%s"}',
    user_id, profile_name
  ))

  if (nrow(profile) == 0) {
    return(list(success = FALSE, message = "Profile not found"))
  }

  # Parse the JSON profile data
  profile_data <- tryCatch({
    jsonlite::fromJSON(profile$profile_data[1], simplifyVector = FALSE)
  }, error = function(e) {
    NULL
  })

  if (is.null(profile_data)) {
    return(list(success = FALSE, message = "Error parsing profile data"))
  }

  list(success = TRUE, profile_data = profile_data)
}

#' List all profiles for a user
#' @param user_id User's MongoDB ID
#' @return Data frame with profile names and timestamps
list_user_profiles <- function(user_id) {
  profiles <- mongo_connect("profiles")
  if (is.null(profiles)) {
    return(data.frame(profile_name = character(), updated_at = character()))
  }
  on.exit(profiles$disconnect(), add = TRUE)

  result <- profiles$find(
    sprintf('{"user_id": "%s"}', user_id),
    fields = '{"profile_name": 1, "updated_at": 1, "_id": 0}'
  )

  if (nrow(result) == 0) {
    return(data.frame(profile_name = character(), updated_at = character()))
  }

  result[order(result$updated_at, decreasing = TRUE), ]
}

#' Delete a profile from MongoDB
#' @param user_id User's MongoDB ID
#' @param profile_name Name of the profile to delete
#' @return List with success status and message
delete_profile_from_db <- function(user_id, profile_name) {
  profiles <- mongo_connect("profiles")
  if (is.null(profiles)) {
    return(list(success = FALSE, message = "Database not available"))
  }
  on.exit(profiles$disconnect(), add = TRUE)

  result <- profiles$remove(sprintf(
    '{"user_id": "%s", "profile_name": "%s"}',
    user_id, profile_name
  ))

  if (result$removed > 0) {
    list(success = TRUE, message = "Profile deleted")
  } else {
    list(success = FALSE, message = "Profile not found")
  }
}

#' Check if MongoDB is available
#' @return TRUE if connected, FALSE otherwise
is_mongodb_available <- function() {
  conn <- mongo_connect("users")
  if (is.null(conn)) return(FALSE)
  tryCatch({
    conn$count()  # Simple operation to verify connection
    conn$disconnect()
    TRUE
  }, error = function(e) {
    FALSE
  })
}

# ============================================================================

# Centralized version info - robust for all launch methods
`%||%` <- function(x, y) if (is.null(x)) y else x

# Find VERSION.md using multiple strategies
find_version_file <- function() {
  # Strategy 1: Current working directory (most reliable for Shiny apps)
  cwd_path <- file.path(getwd(), "VERSION.md")
  if (file.exists(cwd_path)) return(cwd_path)

  # Strategy 2: Relative to this file (if available)
  script_path <- tryCatch({
    script_dir <- dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
    file.path(script_dir, "VERSION.md")
  }, error = function(e) NULL)
  if (!is.null(script_path) && file.exists(script_path)) return(script_path)

  # Strategy 3: Search up directory tree from getwd()
  cur <- getwd()
  for (i in 1:5) {
    cand <- file.path(cur, "VERSION.md")
    if (file.exists(cand)) return(cand)
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  NULL
}

read_version_direct <- function() {
  ver <- "dev"
  dt <- as.character(Sys.Date())
  sha <- "unknown"
  tag <- ""

  # Try environment variables first (set by launch_mapper.R)
  env_ver <- Sys.getenv("CVALR_APP_VERSION", "")
  if (nzchar(env_ver)) {
    ver <- env_ver
    dt <- Sys.getenv("CVALR_APP_VERSION_DATE", dt)
    sha <- Sys.getenv("CVALR_APP_BUILD_SHA", sha)
    tag <- Sys.getenv("CVALR_APP_BUILD_TAG", tag)
    return(list(version = ver, date = dt, sha = sha, tag = tag))
  }

  # Read VERSION.md directly
  vfile <- find_version_file()
  if (!is.null(vfile) && file.exists(vfile)) {
    lines <- tryCatch(readLines(vfile, warn = FALSE), error = function(e) character())
    if (length(lines)) {
      idx <- grep("Current Version:", lines, fixed = TRUE)
      if (length(idx)) {
        vv <- sub(".*Current Version:\\s*", "", lines[idx[1]])
        vv <- gsub("\\*", "", vv)
        vv <- trimws(vv)
        if (grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", vv)) ver <- vv
      }
      idx2 <- grep("Last Updated:", lines, fixed = TRUE)
      if (length(idx2)) {
        dd <- sub(".*Last Updated:\\s*", "", lines[idx2[1]])
        dd <- gsub("\\*", "", dd)
        dd <- trimws(dd)
        if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dd)) dt <- dd
      }
    }
  }

  # Try git for SHA/tag
  git <- Sys.which("git")
  if (nzchar(git)) {
    sha_out <- tryCatch(
      system2(git, c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    if (length(sha_out) && nzchar(sha_out[1])) sha <- sha_out[1]

    tag_out <- tryCatch(
      system2(git, c("describe", "--tags", "--abbrev=0"), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    if (length(tag_out) && nzchar(tag_out[1])) tag <- tag_out[1]
  }

  list(version = ver, date = dt, sha = sha, tag = tag)
}

ver <- read_version_direct()
APP_VERSION <- ver$version
APP_VERSION_DATE <- ver$date
APP_SHA <- ver$sha
APP_TAG <- ver$tag

format_build_label <- function() {
  tag_part <- if (nzchar(APP_TAG)) paste0(", ", APP_TAG) else ""
  sprintf("CAA Column Mapper v%s (%s%s)", APP_VERSION, APP_SHA, tag_part)
}

DERIVED_METRIC_PRICE_PER_SQFT <- "PricePerSqFt"
DERIVED_METRIC_PRICE_PER_LOTSF <- "PricePerLotSF"
DERIVED_METRIC_SALE_QTR <- "SaleQtr"
DERIVED_METRIC_FULL_ADDRESS <- "FullAddress"
DERIVED_METRIC_STREET_ADDRESS <- "StreetAddress"
DERIVED_METRIC_COLUMNS <- c(
  DERIVED_METRIC_PRICE_PER_SQFT,
  DERIVED_METRIC_PRICE_PER_LOTSF,
  DERIVED_METRIC_SALE_QTR,
  DERIVED_METRIC_FULL_ADDRESS,
  DERIVED_METRIC_STREET_ADDRESS
)

# ============================================================================
# DERIVED METRIC REQUIREMENTS
# ============================================================================
# Defines what source columns are needed to derive each metric
# Used during mapping phase to offer derivation when destination column is missing
# ============================================================================

DERIVED_METRIC_REQUIREMENTS <- list(
  SaleQtr = list(
    label = "Sale Quarter",
    description = "Year.Quarter format (e.g., 2024.3 for Q3 2024)",
    required_any = c("DateSold", "SaleDate", "SoldDate", "CloseDate", "ClosingDate", "DateClose"),
    formula = "Calculated from sale date: Year + (Quarter / 10)"
  ),
  PricePerSqFt = list(
    label = "Price Per Sq Ft",
    description = "Sale price divided by living area square footage",
    required_all = c("PriceSold", "SqFtTotal"),
    formula = "PriceSold / SqFtTotal"
  ),
  PricePerLotSF = list(
    label = "Price Per Lot SF",
    description = "Sale price divided by lot size in square feet",
    required_all = c("PriceSold", "LotS"),
    formula = "PriceSold / LotS"
  ),
  FullAddress = list(
    label = "Full Address",
    description = "Complete address with city, state, and ZIP",
    required_all = c("StreetNumber", "StreetName"),
    optional = c("StreetDirPrefix", "StreetSuffix", "StreetDirSuffix", "UnitNumber", "City", "StateOrProvince", "PostalCode"),
    formula = "Assembled from address components"
  ),
  StreetAddress = list(
    label = "Street Address",
    description = "Street address only (no city/state/ZIP)",
    required_all = c("StreetNumber", "StreetName"),
    optional = c("StreetDirPrefix", "StreetSuffix", "StreetDirSuffix", "UnitNumber"),
    formula = "Assembled from street components"
  )
)

#' Check if a derived metric can be created from available mappings
#'
#' @param metric_name The name of the derived metric (e.g., "SaleQtr")
#' @param current_mappings Named list of current target -> source mappings
#' @return List with can_derive (boolean), missing_fields (character vector), available_fields (character vector)
check_derivability <- function(metric_name, current_mappings) {
  req <- DERIVED_METRIC_REQUIREMENTS[[metric_name]]
  if (is.null(req)) {
    return(list(can_derive = FALSE, reason = "Unknown metric", missing_fields = character(), available_fields = character()))
  }

  # Normalize mapping keys for comparison
  mapping_targets <- names(current_mappings)
  normalize_name <- function(x) tolower(gsub("[^[:alnum:]]", "", x))
  mapping_targets_norm <- sapply(mapping_targets, normalize_name)

  # Check required_any (at least one of these must be mapped)
  if (!is.null(req$required_any)) {
    any_norm <- sapply(req$required_any, normalize_name)
    matched_any <- any(any_norm %in% mapping_targets_norm)
    if (!matched_any) {
      return(list(
        can_derive = FALSE,
        reason = sprintf("Need one of: %s", paste(req$required_any, collapse = ", ")),
        missing_fields = req$required_any,
        available_fields = character()
      ))
    }
    available <- req$required_any[any_norm %in% mapping_targets_norm]
    return(list(can_derive = TRUE, reason = NULL, missing_fields = character(), available_fields = available))
  }

  # Check required_all (all of these must be mapped)
  if (!is.null(req$required_all)) {
    all_norm <- sapply(req$required_all, normalize_name)
    matched_all <- all_norm %in% mapping_targets_norm
    missing <- req$required_all[!matched_all]
    available <- req$required_all[matched_all]

    if (!all(matched_all)) {
      return(list(
        can_derive = FALSE,
        reason = sprintf("Missing: %s", paste(missing, collapse = ", ")),
        missing_fields = missing,
        available_fields = available
      ))
    }
    return(list(can_derive = TRUE, reason = NULL, missing_fields = character(), available_fields = available))
  }

  return(list(can_derive = FALSE, reason = "No requirements defined", missing_fields = character(), available_fields = character()))
}

#' Get derivability status for a destination column
#'
#' @param dest_col Destination column name to check
#' @param current_mappings Named list of current target -> source mappings
#' @return List with is_derivable, metric_name, label, description, formula, derivability_check
get_derived_metric_for_dest <- function(dest_col, current_mappings) {
  # Normalize the destination column name for matching
  dest_norm <- tolower(gsub("[^[:alnum:]]", "", dest_col))

  # Check if this destination column matches any derivable metric
  for (metric_name in names(DERIVED_METRIC_REQUIREMENTS)) {
    metric_norm <- tolower(gsub("[^[:alnum:]]", "", metric_name))
    if (dest_norm == metric_norm) {
      req <- DERIVED_METRIC_REQUIREMENTS[[metric_name]]
      derivability <- check_derivability(metric_name, current_mappings)
      return(list(
        is_derivable = TRUE,
        metric_name = metric_name,
        label = req$label,
        description = req$description,
        formula = req$formula,
        can_derive = derivability$can_derive,
        reason = derivability$reason,
        missing_fields = derivability$missing_fields,
        available_fields = derivability$available_fields
      ))
    }
  }

  return(list(is_derivable = FALSE))
}

# ============================================================================
# ADDRESS ASSEMBLY FUNCTION
# ============================================================================
# Assembles address components into a formatted address string following
# RESO standard order: StreetNumber, DirPrefix, StreetName, Suffix, DirSuffix, Unit
#
# Reference: docs/PROMPT_ADDRESS_ASSEMBLY_FEATURE.md
# ============================================================================

#' Assemble street address from components
#'
#' @param data Data frame containing address component columns
#' @param street_number_col Column name for street number (e.g., "StreetNumber", "StreetNumberNumeric")
#' @param street_dir_prefix_col Column name for direction prefix (e.g., "StreetDirPrefix") - N, S, E, W
#' @param street_name_col Column name for street name (e.g., "StreetName")
#' @param street_suffix_col Column name for street suffix (e.g., "StreetSuffix") - St, Ave, Blvd
#' @param street_dir_suffix_col Column name for direction suffix (e.g., "StreetDirSuffix") - N, S, E, W
#' @param unit_number_col Column name for unit number (e.g., "UnitNumber")
#' @param city_col Column name for city (e.g., "City")
#' @param state_col Column name for state (e.g., "StateOrProvince")
#' @param postal_code_col Column name for ZIP code (e.g., "PostalCode")
#' @param postal_code_plus4_col Column name for ZIP+4 (e.g., "PostalCodePlus4")
#' @param include_city_state_zip Logical; if TRUE, include city, state, ZIP in output
#' @param include_zip_plus4 Logical; if TRUE and available, include ZIP+4
#' @return Character vector of assembled addresses
assemble_address <- function(data,
                              street_number_col = NULL,
                              street_dir_prefix_col = NULL,
                              street_name_col = NULL,
                              street_suffix_col = NULL,
                              street_dir_suffix_col = NULL,
                              unit_number_col = NULL,
                              city_col = NULL,
                              state_col = NULL,
                              postal_code_col = NULL,
                              postal_code_plus4_col = NULL,
                              include_city_state_zip = TRUE,
                              include_zip_plus4 = TRUE) {

  n <- nrow(data)
  addresses <- character(n)

  # Helper to safely get column value, returning empty string if NULL/NA/missing

  safe_get <- function(row_idx, col_name) {
    if (is.null(col_name) || !col_name %in% names(data)) return("")
    val <- data[[col_name]][row_idx]
    if (is.null(val) || is.na(val)) return("")
    val <- trimws(as.character(val))
    if (!nzchar(val)) return("")
    return(val)
  }

  for (i in seq_len(n)) {
    # Build street address in RESO order
    street_parts <- c(
      safe_get(i, street_number_col),      # 1. Street number
      safe_get(i, street_dir_prefix_col),  # 2. Direction prefix (N, S, E, W)
      safe_get(i, street_name_col),        # 3. Street name
      safe_get(i, street_suffix_col),      # 4. Street suffix (St, Ave, etc.)
      safe_get(i, street_dir_suffix_col)   # 5. Direction suffix
    )

    # Filter out empty parts and join with single space
    street_parts <- street_parts[nzchar(street_parts)]
    street_line <- paste(street_parts, collapse = " ")

    # Add unit number if present
    unit <- safe_get(i, unit_number_col)
    if (nzchar(unit)) {
      # Add unit prefix if not already present
      unit_lower <- tolower(unit)
      has_prefix <- grepl("^(apt|unit|suite|ste|#|no\\.?|number)\\s*", unit_lower)
      if (!has_prefix) {
        # Check if it's just a number - add # prefix
        if (grepl("^[0-9]+[a-zA-Z]?$", unit)) {
          unit <- paste("#", unit)
        }
      }
      street_line <- paste(street_line, unit)
    }

    # Add city, state, ZIP if requested
    if (include_city_state_zip) {
      city <- safe_get(i, city_col)
      state <- safe_get(i, state_col)
      zip <- safe_get(i, postal_code_col)
      zip_plus4 <- safe_get(i, postal_code_plus4_col)

      # Build city/state/zip line
      csz_parts <- character(0)

      if (nzchar(city)) {
        csz_parts <- c(csz_parts, city)
      }

      if (nzchar(state)) {
        if (length(csz_parts) > 0) {
          # Add comma after city if present
          csz_parts[length(csz_parts)] <- paste0(csz_parts[length(csz_parts)], ",")
        }
        csz_parts <- c(csz_parts, state)
      }

      if (nzchar(zip)) {
        # Format ZIP with +4 if available and requested
        if (include_zip_plus4 && nzchar(zip_plus4)) {
          zip <- paste0(zip, "-", zip_plus4)
        }
        csz_parts <- c(csz_parts, zip)
      }

      csz_line <- paste(csz_parts, collapse = " ")

      if (nzchar(street_line) && nzchar(csz_line)) {
        addresses[i] <- paste(street_line, csz_line, sep = ", ")
      } else if (nzchar(street_line)) {
        addresses[i] <- street_line
      } else {
        addresses[i] <- csz_line
      }
    } else {
      addresses[i] <- street_line
    }

    # Clean up any double spaces
    addresses[i] <- gsub("\\s+", " ", addresses[i])
    addresses[i] <- trimws(addresses[i])
  }

  return(addresses)
}

#' Auto-detect address component columns in a dataset
#'
#' Looks for common RESO address field names and returns a named list of detected columns
#' @param col_names Character vector of column names
#' @return Named list mapping component type to detected column name
detect_address_columns <- function(col_names) {
  col_lower <- tolower(col_names)

  # Define patterns for each component (in order of preference)
  patterns <- list(
    street_number = c("streetnumber", "streetnumbernumeric", "streetno", "houseno", "housenumber", "stno", "stnum"),
    street_dir_prefix = c("streetdirprefix", "streetdirectionprefix", "strdirprefix", "dirprefix"),
    street_name = c("streetname", "stname", "strname"),
    street_suffix = c("streetsuffix", "strsuffix", "stsuffix", "streettype"),
    street_dir_suffix = c("streetdirsuffix", "streetdirectionsuffix", "strdirsuffix", "dirsuffix"),
    unit_number = c("unitnumber", "unit", "apt", "aptno", "apartment", "suite", "suiteno", "suitenumber"),
    city = c("city", "municipality", "town"),
    state = c("stateorprovince", "state", "province", "st"),
    postal_code = c("postalcode", "zipcode", "zip", "postcode"),
    postal_code_plus4 = c("postalcodeplus4", "zipcodeplus4", "zipplus4", "zip4", "plus4")
  )

  detected <- list()

  for (component in names(patterns)) {
    for (pattern in patterns[[component]]) {
      # Check normalized (no underscores/spaces) match
      col_norm <- gsub("[^[:alnum:]]", "", col_lower)
      pattern_norm <- gsub("[^[:alnum:]]", "", pattern)

      match_idx <- which(col_norm == pattern_norm)
      if (length(match_idx) > 0) {
        detected[[component]] <- col_names[match_idx[1]]
        break
      }
    }
  }

  return(detected)
}

# Global thresholds for auto-suggestions and exact matches
SUGGEST_THRESHOLD <- 65L
EXACT_MATCH_THRESHOLD <- 95L

# Helper functions
is_price_ratio_column_name <- function(name) {
  if (is.null(name) || !nzchar(name)) return(FALSE)

  lower_name <- tolower(trimws(name))
  compact <- gsub("[^a-z0-9]", "", lower_name)

  tokenized <- unlist(strsplit(gsub("[^[:alnum:]]", " ", lower_name), "\\s+"))
  tokens <- unique(tokenized[nzchar(tokenized)])

  ratio_tokens <- c("ratio", "pct", "percent", "percentage", "percentratio", "pctg")
  connector_tokens <- c("to", "vs", "over", "per", "by", "of", "vs.", "over.")
  sold_tokens <- c("sold", "sale", "sales", "close", "closing", "final", "sp", "soldprice", "saleprice", "closeprice")
  list_tokens <- c("list", "listing", "ask", "asking", "lp", "olp", "listprice", "askingprice", "origlist", "original", "olist", "olistprice")

  has_ratio_token <- any(tokens %in% ratio_tokens) || grepl("(ratio|pct|percent)", compact)
  has_pct_symbol <- grepl("%", lower_name, fixed = TRUE)

  has_sp_token <- any(tokens == "sp")
  has_lp_token <- any(tokens %in% c("lp", "olp"))
  sp_lp_compact <- grepl("sp(olp|lp)", compact)

  sold_list_tokens <- any(tokens %in% sold_tokens) && any(tokens %in% list_tokens)
  has_connector <- any(tokens %in% connector_tokens)
  sold_list_with_connector <- sold_list_tokens && has_connector
  sold_list_compact <- grepl("(sale|sold|close|final)[a-z0-9]*(to|vs|over|/)[a-z0-9]*(list|ask|lp|olp|origlist|original)", compact)

  return(
    has_ratio_token ||
      has_pct_symbol ||
      sp_lp_compact ||
      (has_sp_token && has_lp_token) ||
      sold_list_with_connector ||
      sold_list_compact
  )
}

normalize_selection_values <- function(values) {
  if (is.null(values)) return(character())
  vals <- trimws(as.character(values))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (!length(vals)) return(character())
  sort(unique(vals))
}

selections_match <- function(a, b) {
  identical(normalize_selection_values(a), normalize_selection_values(b))
}

# ============================================================================
# DECIMAL BATHS TRANSFORMATION
# ============================================================================
# Converts decimal bath values (e.g., 2.2, 3.1) to separate Full/Half bath counts
# Convention: Integer part = full baths, Decimal part (x10) = half baths
# Example: 2.2 = 2 full, 2 half | 3.1 = 3 full, 1 half | 4.3 = 4 full, 3 half
# ============================================================================

#' Split decimal baths into full and half bath counts with total integer
#'
#' @param decimal_baths Numeric vector of decimal bath values (e.g., 2.2, 3.1)
#' @return List with three numeric vectors: full_baths, half_baths, and total_integer
#' @details Convention: Integer part = full baths, Decimal digit = half bath COUNT
#'   Example: 2.2 = 2 full + 2 half | 3.1 = 3 full + 1 half
#'   BathroomsTotalInteger = Full + Half (simple count per RESO standard)
split_decimal_baths <- function(decimal_baths) {
  # Ensure numeric
  decimal_baths <- as.numeric(decimal_baths)

  # Calculate full baths (integer part)
  full_baths <- floor(decimal_baths)

  # Calculate half baths (decimal part * 10, rounded to handle floating point)
  half_baths <- round((decimal_baths - full_baths) * 10)

  # Calculate BathroomsTotalInteger: Simple count per RESO standard
  # 2.2 (2 full + 2 half) = 4 total bathrooms
  total_integer <- full_baths + half_baths

  # Handle NA values
  full_baths[is.na(decimal_baths)] <- NA
  half_baths[is.na(decimal_baths)] <- NA
  total_integer[is.na(decimal_baths)] <- NA

  return(list(
    full_baths = full_baths,
    half_baths = half_baths,
    total_integer = total_integer
  ))
}

#' Check if a column uses the decimal bath coding system
#'
#' Decimal bath coding: integer part = full baths, decimal digit = half bath COUNT
#'   3.2 = 3 full + 2 half, 4.1 = 4 full + 1 half
#' Detection: if any decimal values are NOT .5 (e.g. .1, .2, .3), it's the coded system.
#'   Values like 2.5, 3.5 alone are just "X and a half baths" (not coded).
#'
#' @param values Vector of values to check
#' @return Logical indicating if values use decimal bath coding
is_decimal_baths <- function(values) {
  num_vals <- suppressWarnings(as.numeric(values))
  num_vals <- num_vals[!is.na(num_vals)]

  if (length(num_vals) == 0) return(FALSE)

  # Must be in reasonable bath range (0-20)
  if (!all(num_vals >= 0 & num_vals <= 20)) return(FALSE)

  # Get the decimal parts
  decimal_parts <- round((num_vals - floor(num_vals)) * 10) / 10
  has_decimals <- decimal_parts[decimal_parts != 0]

  if (length(has_decimals) == 0) return(FALSE)

  # Key test: if ANY decimal is not .5, it's the coded system
  # (e.g. .1, .2, .3 are clearly coded half-bath counts)
  has_non_half <- any(has_decimals != 0.5)

  return(has_non_half)
}

# ============================================================================
# CSV Export Standard: RFC 4180 Compliant
# ============================================================================
# Helper function for robust CSV export that ensures compatibility with:
# - readr::read_csv() (modern R standard)
# - data.table::fread() (high-performance parser)
# - pandas.read_csv() (Python)
# - Other RFC 4180 compliant CSV readers
#
# This prevents parsing failures caused by:
# - Unquoted commas in text fields (legal descriptions, addresses, etc.)
# - Inconsistent quote escaping between base R and modern parsers
# - Line ending differences across platforms
# ============================================================================
export_csv_robust <- function(data, file) {
  # Use write_csv with minimal explicit parameters for maximum compatibility
  # readr automatically handles RFC 4180 compliance by default
  write_csv(
    x = data,
    file = file,
    na = "",                    # Empty string for NA (standard convention)
    quote = "needed",           # Quote only fields that need it (commas, quotes, newlines)
    eol = "\n"                  # Unix line endings (cross-platform safe)
    # Note: escape defaults to "double" (RFC 4180), progress defaults to show_progress()
  )

  # Self-check: verify the exported file can be read back correctly
  # This catches export issues immediately rather than during downstream use
  tryCatch({
    # Test with readr (primary downstream consumer)
    test_readr <- read_csv(file, show_col_types = FALSE)
    if (nrow(test_readr) != nrow(data)) {
      warning(sprintf("CSV export validation: Row count mismatch with readr - expected %d, got %d",
                     nrow(data), nrow(test_readr)))
    }

    # Test with data.table fread (alternative downstream consumer)
    if (requireNamespace("data.table", quietly = TRUE)) {
      test_fread <- data.table::fread(file, data.table = FALSE)
      if (nrow(test_fread) != nrow(data)) {
        warning(sprintf("CSV export validation: Row count mismatch with fread - expected %d, got %d",
                       nrow(data), nrow(test_fread)))
      }
    }
  }, error = function(e) {
    warning(sprintf("CSV export self-check failed: %s", e$message))
  })

  invisible(NULL)
}

# ============================================================================
# STATUS VALUE NORMALIZATION
# ============================================================================
# Maps common MLS status abbreviations to CValR-expected full words.
# Case-insensitive: input is lowercased before lookup.
# Unknown values pass through unchanged.

CVALR_STATUS_MAP <- c(
  "act"     = "Active",
  "active"  = "Active",
  "a"       = "Active",
  "bom"     = "Active",
  "sld"     = "Sold",
  "sold"    = "Sold",
  "s"       = "Sold",
  "cls"     = "Sold",
  "closed"  = "Sold",
  "snl"     = "Sold",
  "pen"     = "Pending",
  "pending" = "Pending",
  "p"       = "Pending",
  "exp"     = "Expired",
  "expired" = "Expired",
  "wdn"     = "Withdrawn",
  "withdrawn" = "Withdrawn",
  "can"     = "Canceled",
  "canceled" = "Canceled",
  "cancelled" = "Canceled"
)

#' Normalize MLS status abbreviations to CValR-expected full words
#' @param status_values Character vector of status values
#' @return List with `values` (normalized character vector) and `warnings` (unrecognized values)
normalize_status <- function(status_values) {
  original <- status_values
  lowered <- tolower(trimws(as.character(status_values)))
  mapped <- CVALR_STATUS_MAP[lowered]
  # Use mapped value where found, otherwise keep original
  result <- ifelse(is.na(mapped), original, mapped)
  # Identify unrecognized non-NA, non-empty values
  unrecognized <- unique(original[is.na(mapped) & !is.na(original) & nzchar(original)])
  list(values = result, warnings = unrecognized)
}

# ============================================================================
# DWELLING TYPE VALUE NORMALIZATION
# ============================================================================
# Maps US MLS abbreviations and RESO PropertySubType values to CValR-expected
# Canadian-standard DwellingType values.
# Case-insensitive: input is lowercased before lookup.
# Unknown values pass through unchanged.

CVALR_DWELLING_MAP <- c(
  # US MLS abbreviations
  "detachd"                          = "Single Family - Detached",
  "detached"                         = "Single Family - Detached",
  "attachd"                          = "Townhouse - Detached",
  "attached"                         = "Townhouse",
  "twnhs"                            = "Townhouse",
  "condo"                            = "Apartment",
  "condmn"                           = "Apartment",
  "condominium"                      = "Apartment",
  "mfr"                              = "Apartment",
  "multires"                         = "Apartment",
  # RESO PropertySubType values
  "single family residence"          = "Single Family - Detached",
  "single family"                    = "Single Family - Detached",
  "manufactured home"                = "Single Family - Detached",
  "townhouse"                        = "Townhouse",
  "residential lot"                  = "Residential Lot",
  "residential acreage"              = "Residential Acreage",
  "lots/acreage"                     = "Residential Lot",
  "vacant land"                      = "Residential Lot",
  "land"                             = "Residential Lot",
  # CValR canonical values (pass-through, ensures case normalization)
  "single family - detached"         = "Single Family - Detached",
  "single family - detached w/acreage" = "Single Family - Detached w/Acreage",
  "townhouse - detached"             = "Townhouse - Detached",
  "townhouse semi-detached"          = "Townhouse Semi-Detached",
  "apartment"                        = "Apartment",
  "timeshare"                        = "Timeshare"
)

#' Normalize DwellingType values to CValR-expected standard values
#' @param dwelling_values Character vector of dwelling type values
#' @return List with `values` (normalized character vector) and `warnings` (unrecognized values)
normalize_dwelling_type <- function(dwelling_values) {
  original <- dwelling_values
  lowered <- tolower(trimws(as.character(dwelling_values)))
  mapped <- CVALR_DWELLING_MAP[lowered]
  # Use mapped value where found, otherwise keep original
  result <- ifelse(is.na(mapped), original, mapped)
  # Identify unrecognized non-NA, non-empty values
  unrecognized <- unique(original[is.na(mapped) & !is.na(original) & nzchar(original)])
  list(values = result, warnings = unrecognized)
}

# ============================================================================
# ID FIELD CATEGORIZATION (RESO Standards)
# ============================================================================
# ListingKey (TransactionKey) = THE unique identifier - one per listing
# ListingId (TransactionID) = MLS number - may not be unique across MLSs

#' Categorize a column as ListingKey-type or ListingId-type based on naming
#' @param col_name The column name to categorize
#' @return "ListingKey", "ListingId", or "Unknown"
categorize_id_column <- function(col_name) {
  col_norm <- tolower(gsub("[^[:alnum:]]", "", col_name))
  col_lower <- tolower(col_name)

  # EXCLUDE: Agent, Office, Broker, Area columns - these are NOT primary listing IDs
  exclude_patterns <- c("agent", "office", "broker", "area", "region", "contact", "owner")
  if (any(sapply(exclude_patterns, function(p) grepl(p, col_lower)))) {
    return("Unknown")
  }

  # ListingKey patterns (unique identifiers / system keys)
  # These are typically system-generated unique IDs
  key_patterns <- c("^uniqueid$", "^listingkey", "transactionkey", "primarykey",
                    "^recordid$", "matrixuniqueid", "^matrixid$", "listingkeynumeric",
                    "listnbr", "listnumber")
  if (any(sapply(key_patterns, function(p) grepl(p, col_norm)))) {
    return("ListingKey")
  }

  # ListingId patterns (MLS numbers - human-readable, may not be unique)
  # Be specific: only match if it looks like a primary listing identifier
  id_patterns <- c("^mls$", "^mlsid$", "^mlsno$", "^mlsnumber$", "^mlsnum$",
                   "^listingid$", "^listingnumber$", "^listingno$")
  if (any(sapply(id_patterns, function(p) grepl(p, col_norm)))) {
    return("ListingId")
  }

  # Check for "MLS #" pattern (with special characters stripped)
  if (grepl("^mls", col_norm) && nchar(col_norm) <= 6) {
    return("ListingId")
  }

  return("Unknown")
}

#' Analyze ID columns in a dataset and determine if user prompt is needed
#' @param col_names Vector of column names
#' @param scores Named vector or list of scores for each column against ID fields
#' @return List with candidates, single_id flag, and needs_prompt flag
analyze_id_columns <- function(col_names, id_field_scores) {
  ID_SCORE_THRESHOLD <- 80  # Include pattern-matched columns (base score is 85)

  # Find columns that match ID fields above threshold
  candidates <- data.frame(
    column = character(),
    score = numeric(),
    category = character(),
    stringsAsFactors = FALSE
  )

  for (col in col_names) {
    score <- id_field_scores[[col]] %||% 0
    if (score >= ID_SCORE_THRESHOLD) {
      category <- categorize_id_column(col)
      if (category != "Unknown") {
        candidates <- rbind(candidates, data.frame(
          column = col,
          score = score,
          category = category,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Sort by score descending
  if (nrow(candidates) > 0) {
    candidates <- candidates[order(-candidates$score), ]
  }

  list(
    candidates = candidates,
    single_id = nrow(candidates) == 1,
    needs_prompt = nrow(candidates) >= 2
  )
}

# Define target groups (complete list from main app)
logic_target_groups <- list(
  "Date Fields" = list(
    DateSold = list(label = "Sale Date", required = TRUE, class = 1,
                    synonyms = c("CloseDate","SoldDate","COE","COEDate","sale date","sold date","date sold","closing date","close date","settlement date"), type = "date"),
    DateListed = list(label = "Listing Date", class = 1, synonyms = c("ListingContractDate","ListingDate","ListDate","OnMarketDate","AgreementDate","list date","listing date","on market date"), type = "date"),
    DatePending = list(label = "Pending Date", class = 2, synonyms = c("PurchaseContractDate","PendingDate","UnderContractDate","ContractDate","Pend","pending date","offer date","contract date","agreement date","accepted offer date","acceptance date","pend","pend date"), type = "date"),
    OffMarketDate = list(label = "Off Market", class = 2, synonyms = c("OffMarketDate","DateOffMarket","WithdrawnDate","ExpirationDate","off market","withdrawn date","expiry date","expiration date","expired date","cancel date","cancelled date"), type = "date"),
    TitleChangeDate = list(label = "Title/Ownership Change Date", class = 2, synonyms = c("title/ownership change date","title change date","ownership change date","title transfer date","deed date","recording date"), type = "date")
  ),
  "Price Fields" = list(
    PriceSold = list(label = "Sold Price", required = TRUE, class = 1, synonyms = c("ClosePrice","SoldPrice","SalePrice","SellingPrice","PurchasePrice","sale price","sold price","close price","final price","selling price"), type = "currency"),
    PriceListed = list(label = "List Price", class = 1, synonyms = c("ListPrice","AskingPrice","PriceListing","listing price","list price","asking price","ask price"), type = "currency"),
    PriceOriginal = list(label = "Original Price", class = 2, synonyms = c("OriginalListPrice","OriginalPrice","original list price","original price","initial list price"), type = "currency")
  ),
  "Location" = list(
    Address = list(label = "Full Address", class = 1, synonyms = c("Address","FullAddress","UnparsedAddress","address","street address","property address","full address","site address"), type = "text"),
    City = list(label = "City", class = 1, synonyms = c("City","municipality","town"), type = "text"),
    PostalCode = list(label = "Postal Code", class = 1, synonyms = c("PostalCode","ZipCode","Zip","zip code","postal code","postal"), type = "text"),
    Latitude = list(label = "Latitude", class = 2, synonyms = c("Latitude","lat"), type = "number"),
    Longitude = list(label = "Longitude", class = 2, synonyms = c("Longitude","lon","lng","long"), type = "number"),
    Neighbourhood = list(label = "Neighbourhood", class = 2, synonyms = c("neighborhood","neighbourhood","district","community"), type = "text"),
    SubArea = list(label = "Sub Area", class = 2, synonyms = c("SubArea","MLSAreaMinor","sub area","mls area minor"), type = "text"),
    Region = list(label = "Region", class = 2, synonyms = c("CountyOrParish","County","region","county","parish"), type = "text")
  ),
  "Identifiers" = list(
    ListingKey = list(label = "Listing Key (Unique ID)", required = TRUE, class = 1, synonyms = c("ListingKey","ListingKeyNumeric","Matrix_Unique_ID","listing key","unique id","transaction key","primary key","record id","system id"), type = "id"),
    ListingId = list(label = "MLS / Listing ID", class = 1, synonyms = c("ListingId","MLS","mls id","mls number","mls no","mls#","listing id","listing number"), type = "id"),
PID = list(label = "Property ID (PID)", class = 2, synonyms = c("ParcelNumber","APN","PID","pid","parcel","parcel id","parcel number"), type = "id"),
    Status = list(label = "Listing Status", class = 1, synonyms = c("StandardStatus","MlsStatus","listing status","current status","mls status","status"), type = "text"),
    LegalDescription = list(label = "Legal Description", class = 2, synonyms = c("legal description","legal desc","legal","ldesc","lot legal","plan","block","lot","parcel plan","legal text","title legal","tax legal description"), type = "text"),
    TitleHeld = list(label = "Title Held", class = 2, synonyms = c("title held","title","ownership type","title type","how title held"), type = "text"),
    SpecialListingConditions = list(label = "Special Listing Conditions", class = 2, synonyms = c("special conditions","listing conditions","special listing conditions","sale conditions"), type = "text")
  ),
  "Living Area / Square Footage" = list(
    SqFtTotal = list(label = "Total Finished SqFt", class = 1, synonyms = c("LivingArea","BuildingAreaTotal","sqft","square feet","living area","floor area","finished area","total finished sqft","total finished","total sqft","total sf"), type = "number",
                     multiple = TRUE, combine = "sum",
                     derive_option = list(enabled = TRUE, sources = c("AboveGradeFinishedArea", "BelowGradeFinishedArea"), transform = "sum_grade_areas")),
    AboveGradeFinishedArea = list(label = "Above Grade Finished Area", class = 2, synonyms = c("above grade finished","above grade sqft","above grade finished sqft","above grade finished area","abovegradefinishedarea","above grade area","above grade sf"), type = "number",
                                  multiple = TRUE, combine = "sum"),
    BelowGradeFinishedArea = list(label = "Below Grade Finished Area", class = 2, synonyms = c("below grade finished","below grade sqft","below grade finished sqft","below grade finished area","belowgradefinishedarea","below grade area","below grade sf","finished basement sqft"), type = "number",
                                  multiple = TRUE, combine = "sum")
  ),
  "Property Attributes" = list(
    Beds = list(label = "Bedrooms", class = 1, synonyms = c("BedroomsTotal","Bedrooms","beds","bed","br","bd"), type = "number"),
    BA = list(label = "Full Baths", class = 1, synonyms = c("BathroomsFull","FullBaths","Total Baths","BathsTotal","TotalBaths","Baths","baths","total baths","bath count"), type = "number",
              decimal_baths_split = TRUE),
    PB = list(label = "Half Baths", class = 2, synonyms = c("BathroomsHalf","HalfBaths"), type = "number"),
    EnsBa = list(label = "Ensuite Baths", class = 2, synonyms = c("ensuite","master bath","ensuite bath"), type = "number"),
    TotalKitchens = list(label = "Total Kitchens", class = 2, synonyms = c("kitchens","total kitchens"), type = "number"),
    GarSp = list(label = "Garage Spaces", class = 2, synonyms = c("GarageSpaces","ParkingTotal","garage spaces","parking spaces","garage","parking"), type = "number"),
    DwellingType = list(label = "Dwelling Type", class = 2, synonyms = c("dwelling type","home type","PropertyType","property type"), type = "text"),
    StyleStoreys = list(label = "Style/Storeys", class = 2, synonyms = c("StoriesTotal","stories","storeys","number of stories","architectural style"), type = "text"),
    Levels = list(label = "Levels", class = 2, synonyms = c("Levels","levels","number of levels","stories total","# levels"), type = "text"),
    Condition = list(label = "Condition", class = 2, synonyms = c("PropertyCondition","Condition","condition","property condition"), type = "text"),
    Construction = list(label = "Construction", class = 2, synonyms = c("construction","construction type"), type = "text"),
    Zoning = list(label = "Zoning", class = 2, synonyms = c("Zoning","zone","zoning code"), type = "text"),
    View = list(label = "View", class = 2, synonyms = c("View","ViewYN","view description","view type"), type = "text"),
    SewerType = list(label = "Sewer Type", class = 2, synonyms = c("Sewer","sewer","sewer type"), type = "text"),
    WaterInfluence = list(label = "Water Influence", class = 2, synonyms = c("waterfront","lake","river","waterfront features","waterfrontfeatures","lakefront features","lakefrontfeatures","oceanfront features","oceanfrontfeatures","riverfront features","riverfrontfeatures"), type = "text"),
    WaterfrontYN = list(label = "Waterfront Yes/No", class = 2, synonyms = c("waterfront yn","waterfront y/n","waterfrontyn","lakefront","oceanfront","riverfront","on water","is waterfront","has waterfront"), type = "boolean"),
    WaterFrontage = list(label = "Water Frontage", class = 2, synonyms = c("water frontage","waterfront footage"), type = "text"),
    WaterSupply = list(label = "Water Supply", class = 2, synonyms = c("WaterSupply","water supply"), type = "text"),
    WaterSource = list(label = "Water Source", class = 2, synonyms = c("WaterSource","water source"), type = "text"),
    YrBlt = list(label = "Year Built", class = 1, synonyms = c("YearBuilt","year built","built","yr built"), type = "number"),
    LotS = list(label = "Lot Size", required = FALSE, class = 1, type = "number",
                synonyms = c("LotSizeSquareFeet","LotSizeArea","Lot","LotSize","lot size","lot area","lot sqft","land size","lot square feet")),
    LotDimensions = list(label = "Lot Dimensions", class = 2, synonyms = c("lot dimensions","lot size dimensions","lot dim","lot size dimensions l x w","lot size dim"), type = "text"),
    PoolYN = list(label = "Pool", class = 2, synonyms = c("PoolPrivateYN","PoolYN","pool","swimming pool"), type = "boolean"),
    StrataFees = list(label = "Strata Fees", class = 2, type = "currency",
                      synonyms = c("AssociationFee","HOAFee","strata fees","strata fee","hoa fees","hoa fee","hoa dues","hoa","condo fees","condo fee","association fee","association fees"))
  ),
  "Basement & Features" = list(
    BasementYN = list(label = "Basement Yes/No", class = 2, synonyms = c("basement yn","basement y/n","basementyn","has basement","is basement"), type = "boolean"),
    BasementFeatures = list(label = "Basement Features", class = 2, synonyms = c("basement features","basement"), type = "text")
  ),
  "Green / Energy" = list(
    GreenEnergyYN = list(label = "Green/Energy Yes/No", class = 2, synonyms = c("green energy yn","green/energy yn","green energy y/n","green energy","green certified","green certified yn","energy certified","green building"), type = "boolean"),
    GreenEnergyDesc = list(label = "Green/Energy Description", class = 2, synonyms = c("green energy description","green/energy energy efficiency","green energy efficiency","energy efficiency","green/energy public remarks","green energy remarks","green features","energy features"), type = "text")
  ),
  "Remarks & Notes" = list(
    RemarksSales = list(label = "Sales Remarks", required = FALSE, class = 2, type = "text", synonyms = c("PrivateRemarks","private remarks","agent remarks","broker remarks","confidential remarks")),
    RemarksPublic = list(label = "Public Remarks", required = FALSE, class = 2, type = "text", synonyms = c("PublicRemarks","public remarks","description","listing description","property description"))
  )
)

# ============================================================================
# USER-DEFINED SYNONYM MANAGEMENT
# ============================================================================
USER_SYNONYMS_FILE <- "user_synonyms.json"

# Store ORIGINAL synonyms before any user modifications (for field lookup fallback)
# This is populated BEFORE load_user_synonyms() runs
ORIGINAL_SYNONYMS <- list()

#' Build a flat lookup of original synonyms for field identification
#' Called once at startup before user synonyms are loaded
#' @return Named list mapping normalized synonyms to field names
build_original_synonym_lookup <- function() {
  lookup <- list()
  for (gn in names(logic_target_groups)) {
    for (field_name in names(logic_target_groups[[gn]])) {
      meta <- logic_target_groups[[gn]][[field_name]]
      if (!is.null(meta$synonyms)) {
        for (syn in meta$synonyms) {
          syn_norm <- tolower(gsub("[^[:alnum:]]", "", syn))
          lookup[[syn_norm]] <- field_name
        }
      }
      # Also add the field name itself
      field_norm <- tolower(gsub("[^[:alnum:]]", "", field_name))
      lookup[[field_norm]] <- field_name
    }
  }
  lookup
}

# Build BEFORE loading user synonyms
ORIGINAL_SYNONYMS <- build_original_synonym_lookup()

#' Load user-defined synonyms and REPLACE in logic_target_groups
#' User synonyms OVERWRITE original synonyms (allows editing out bad data)
#' @return Boolean indicating success
load_user_synonyms <- function() {
  if (!file.exists(USER_SYNONYMS_FILE)) return(FALSE)

  tryCatch({
    user_syns <- jsonlite::fromJSON(USER_SYNONYMS_FILE)
    count <- 0

    # Iterate through groups and fields to find matches
    for (gn in names(logic_target_groups)) {
      for (field in names(logic_target_groups[[gn]])) {
        if (field %in% names(user_syns)) {
          new_syns <- user_syns[[field]]
          if (length(new_syns) > 0) {
             # REPLACE: user synonyms overwrite original (allows removing bad data)
             logic_target_groups[[gn]][[field]]$synonyms <<- new_syns
             count <- count + 1
          }
        }
      }
    }
    if (count > 0) {
      message(sprintf("Loaded user synonyms for %d fields (replaced originals)", count))
      return(TRUE)
    }
  }, error = function(e) {
    message("Error loading user synonyms: ", e$message)
    return(FALSE)
  })
  return(FALSE)
}

#' Save user-defined synonyms for a specific field
#' @param field_name The target field name (e.g., "ListingId")
#' @param synonyms_list Character vector of synonyms
#' @return Boolean indicating success
save_user_synonyms <- function(field_name, synonyms_list) {
  # Clean input
  synonyms_list <- trimws(synonyms_list)
  synonyms_list <- synonyms_list[nzchar(synonyms_list)]
  
  # Load existing file or create new list
  all_syns <- list()
  if (file.exists(USER_SYNONYMS_FILE)) {
    tryCatch({
      all_syns <- jsonlite::fromJSON(USER_SYNONYMS_FILE)
    }, error = function(e) {
      warning("Could not parse existing user synonyms file, starting fresh.")
    })
  }
  
  # Update specific field
  all_syns[[field_name]] <- synonyms_list
  
  # Write back to file
  tryCatch({
    abs_path <- file.path(getwd(), USER_SYNONYMS_FILE)
    message(sprintf("[SYNONYMS] Saving to: %s", abs_path))
    jsonlite::write_json(all_syns, USER_SYNONYMS_FILE, auto_unbox = FALSE, pretty = TRUE)
    
    # Update memory immediately
    for (gn in names(logic_target_groups)) {
      if (field_name %in% names(logic_target_groups[[gn]])) {
        logic_target_groups[[gn]][[field_name]]$synonyms <<- synonyms_list
        break
      }
    }
    return(TRUE)
  }, error = function(e) {
    message("Error saving user synonyms: ", e$message)
    return(FALSE)
  })
}

# Load synonyms at startup
load_user_synonyms()

# Build synonym -> classification lookup from logic_target_groups
# This allows classification to leverage the same synonyms used for mapping
build_synonym_classification_lookup <- function() {
  lookup <- list()
  for (group_name in names(logic_target_groups)) {
    group <- logic_target_groups[[group_name]]
    for (target_name in names(group)) {
      field_def <- group[[target_name]]
      field_class <- field_def$class  # May be NULL
      if (!is.null(field_class)) {
        # Add target name itself (normalized: lowercase, alphanumeric only)
        normalized <- tolower(gsub("[^[:alnum:]]", "", target_name))
        lookup[[normalized]] <- field_class

        # Add all synonyms
        if (!is.null(field_def$synonyms)) {
          for (syn in field_def$synonyms) {
            syn_normalized <- tolower(gsub("[^[:alnum:]]", "", syn))
            lookup[[syn_normalized]] <- field_class
          }
        }
      }
    }
  }
  lookup
}

# Initialize the synonym-to-classification lookup (built once at startup)
SYNONYM_CLASSIFICATION_LOOKUP <- build_synonym_classification_lookup()

# ============================================================================
# RESO Data Dictionary Definitions
# Load RESO field definitions and synonyms from JSON for tooltips and matching
# ============================================================================

load_reso_definitions <- function() {
  json_path <- file.path(getwd(), "templates", "RESO_Data_Dictionary_Full.json")
  if (file.exists(json_path)) {
    tryCatch({
      jsonlite::fromJSON(json_path, simplifyVector = FALSE)
    }, error = function(e) {
      message("Warning: Could not load RESO definitions: ", e$message)
      list()
    })
  } else {
    list()
  }
}

# Load RESO definitions at startup
RESO_DEFINITIONS <- load_reso_definitions()

# Helper function to get RESO definition for a field
get_reso_definition <- function(field_name) {
  if (is.null(field_name) || !nzchar(field_name)) return(NULL)
  def <- RESO_DEFINITIONS[[field_name]]
  if (!is.null(def)) def$definition else NULL
}

# Build RESO synonym lookup for improved matching
build_reso_synonym_lookup <- function() {
  lookup <- new.env(hash = TRUE, parent = emptyenv())
  for (field_name in names(RESO_DEFINITIONS)) {
    field <- RESO_DEFINITIONS[[field_name]]
    # Map field name to itself (normalized)
    lookup[[tolower(field_name)]] <- field_name
    # Map each synonym to the standard name
    if (!is.null(field$synonyms)) {
      syns <- field$synonyms
      # Handle both string and array formats
      if (is.character(syns)) {
        for (syn in syns) {
          if (nzchar(syn)) {
            syn_normalized <- tolower(gsub("[^[:alnum:]]", "", syn))
            if (nzchar(syn_normalized)) {
              lookup[[syn_normalized]] <- field_name
            }
          }
        }
      }
    }
  }
  lookup
}

# Initialize RESO synonym lookup at startup
RESO_SYNONYM_LOOKUP <- build_reso_synonym_lookup()

message(sprintf("Loaded RESO Data Dictionary: %d fields, %d synonyms",
                length(RESO_DEFINITIONS),
                length(ls(RESO_SYNONYM_LOOKUP))))

target_group_lookup <- unlist(lapply(names(logic_target_groups), function(gn) {
  grp <- logic_target_groups[[gn]]
  setNames(rep(gn, length(grp)), names(grp))
}))

build_standard_field_choices <- function() {
  base_choice <- list("Leave Unassigned / Blank" = c("Unassigned" = ""))
  group_choices <- lapply(names(logic_target_groups), function(gn) {
    grp <- logic_target_groups[[gn]]
    labels <- vapply(names(grp), function(tgt) {
      meta <- grp[[tgt]]
      lbl <- meta$label %||% tgt
      sprintf("%s — %s", lbl, gn)
    }, character(1))
    setNames(names(grp), labels)
  })
  names(group_choices) <- names(logic_target_groups)
  c(base_choice, group_choices)
}

describe_standard_field <- function(target) {
  for (gn in names(logic_target_groups)) {
    grp <- logic_target_groups[[gn]]
    if (target %in% names(grp)) {
      meta <- grp[[target]]
      lbl <- meta$label %||% target
      return(sprintf("%s (%s)", lbl, gn))
    }
  }
  target
}

# Field name aliases for backward compatibility
# Maps legacy/alternate field names to canonical config field names
FIELD_ALIASES <- list(
  # MLS / Listing ID aliases
  MLSNo = "MLS",
  ListingID = "MLS",
  MLSID = "MLS",
  MLSNumber = "MLS",
  ListingNumber = "MLS",
  "Listing ID" = "MLS",
  "MLS Number" = "MLS",
  "Listing Number" = "MLS",
  # List Price aliases
  "List Price" = "PriceListed",
  ListPrice = "PriceListed",
  "Listing Price" = "PriceListed",
  ListingPrice = "PriceListed",
  # Add other common aliases
  "Property Address" = "Address",
  PropertyAddress = "Address",
  "ZIP Code" = "Zip",
  ZIPCode = "ZIP",
  "Zip Code" = "Zip",
  ZipCode = "Zip",
  "Square Footage" = "SquareFeet",
  SquareFootage = "SquareFeet",
  "Property Type" = "PropertyType",
  PropertyType = "PropertyType",
  "Listing Status" = "Status",
  ListingStatus = "Status"
  # Add other aliases as discovered
)

# Resolve field name using aliases (returns canonical name or original if no alias)
resolve_field_alias <- function(field_name) {
  canonical <- FIELD_ALIASES[[field_name]]
  if (!is.null(canonical)) {
    message(sprintf("Resolved alias: '%s' -> '%s'", field_name, canonical))
    return(canonical)
  }
  return(field_name)
}

# Helper function to generate input IDs
# Note: For destination mode, call with dest_mode=TRUE and it will use make.names
id_for <- function(tgt, dest_mode = FALSE) {
  if (dest_mode) {
    paste0("dest_", make.names(tgt), "_selector")
  } else {
    paste0("map_", tgt)
  }
}

# Global suggestion function that assigns each source column to best target
# Returns a named list with: target_field -> list(column=source_column, score=confidence_score)
suggest_all_mappings <- function(target_groups, detected_cols, dataset = NULL, already_mapped = list(), exclusions = list(), classifications = NULL) {
  # TIMING: Track performance
  timing_start <- Sys.time()
  timing_log <- list()

  if (!length(detected_cols)) return(list())

  # Filter out Class 4 columns (PII/internal) from suggestions - they should never be suggested
  if (!is.null(classifications) && is.data.frame(classifications) && nrow(classifications) > 0) {
    class4_cols <- classifications$column_name[!is.na(classifications$class) & classifications$class == 4]
    if (length(class4_cols) > 0) {
      detected_cols <- setdiff(detected_cols, class4_cols)
      message(sprintf("Filtered out %d Class 4 columns from suggestion candidates", length(class4_cols)))
    }
  }
  if (!length(detected_cols)) return(list())

  # Get columns that are already mapped (e.g., from profile)
  already_used_sources <- unique(unlist(already_mapped))
  message(sprintf("Excluding %d already-mapped columns from suggestions", length(already_used_sources)))

  # DEBUG: Log exclusions
  if (length(exclusions) > 0) {
    message(sprintf("=== EXCLUSIONS ACTIVE: %d fields with exclusions ===", length(exclusions)))
    for (fld in names(exclusions)) {
      message(sprintf("  %s: excluding [%s]", fld, paste(exclusions[[fld]], collapse=", ")))
    }
  } else {
    message("=== NO EXCLUSIONS (empty list passed) ===")
  }

  # Get all target fields
  all_targets <- list()
  for (gn in names(target_groups)) {
    for (tgt in names(target_groups[[gn]])) {
      all_targets[[tgt]] <- target_groups[[gn]][[tgt]]
    }
  }

  # PERFORMANCE OPTIMIZATION: Pre-compute column metadata ONCE for all targets
  # This avoids repeated string parsing in calculate_column_scores
  t1 <- Sys.time()
  col_meta <- precompute_column_metadata(detected_cols)
  timing_log$precompute <- as.numeric(difftime(Sys.time(), t1, units = "secs"))

  # Build score matrix: rows = targets, cols = source columns
  # Each cell = how well that source column matches that target
  score_matrix <- matrix(0, nrow = length(all_targets), ncol = length(detected_cols),
                        dimnames = list(names(all_targets), detected_cols))

  # Calculate scores for each target-source pair
  t2 <- Sys.time()
  for (tgt in names(all_targets)) {
    meta <- all_targets[[tgt]]
    # Filter out excluded columns for this specific target
    excluded_for_target <- exclusions[[tgt]] %||% character()
    available_cols <- setdiff(detected_cols, excluded_for_target)
    if (length(available_cols) == 0) {
      # All columns excluded for this target - leave scores at 0
      next
    }

    # Create filtered col_meta for available columns only
    avail_idx <- which(detected_cols %in% available_cols)
    avail_col_meta <- list(
      cols = col_meta$cols[avail_idx],
      norm = col_meta$norm[avail_idx],
      lower = col_meta$lower[avail_idx],
      words = col_meta$words[avail_idx],
      is_boolean = col_meta$is_boolean[avail_idx]
    )

    scores <- calculate_column_scores(tgt, meta, available_cols, dataset, avail_col_meta)
    # Map scores back to full column set (excluded columns get score 0)
    score_matrix[tgt, available_cols] <- scores
  }
  timing_log$scoring <- as.numeric(difftime(Sys.time(), t2, units = "secs"))

  # Greedy assignment: assign best matches first
  assignments <- list()
  used_sources <- character(0)

  # Sort all possible assignments by score (highest first)
  all_scores <- data.frame(
    target = rep(rownames(score_matrix), each = ncol(score_matrix)),
    source = rep(colnames(score_matrix), times = nrow(score_matrix)),
    score = as.vector(t(score_matrix)),
          stringsAsFactors = FALSE
        )
  all_scores <- all_scores[order(-all_scores$score), ]

  # Minimum confidence threshold - only suggest if score >= SUGGEST_THRESHOLD
  # This prevents poor matches from being suggested
  MIN_CONFIDENCE <- SUGGEST_THRESHOLD

  # Initialize with already-mapped sources
  used_sources <- already_used_sources

  # Also initialize assignments with already-mapped targets (from profile)
  # so we don't suggest for fields that already have profile mappings
  assignments <- already_mapped

  # Pre-compute fill rates for ID field validation (only when dataset available)
  id_fill_rates <- list()
  ID_MIN_FILL_RATE <- 0.50  # Reject ID suggestions where source column is <50% populated
  if (!is.null(dataset)) {
    id_targets <- names(which(sapply(all_targets, function(m) identical(m$type, "id"))))
    if (length(id_targets) > 0) {
      for (col in detected_cols) {
        if (col %in% names(dataset)) {
          vals <- dataset[[col]]
          non_empty <- sum(!is.na(vals) & nzchar(trimws(as.character(vals))))
          id_fill_rates[[col]] <- non_empty / max(length(vals), 1)
        }
      }
    }
  }

  # Assign greedily: best score first, skipping already-used sources
  for (i in 1:nrow(all_scores)) {
    tgt <- all_scores$target[i]
    src <- all_scores$source[i]
    score <- all_scores$score[i]

    # Skip if score is too low, source already used, or target already assigned
    if (score < MIN_CONFIDENCE || src %in% used_sources || tgt %in% names(assignments)) {
      next
    }

    # For ID-type fields, reject source columns with very low fill rates
    if (!is.null(all_targets[[tgt]]$type) && identical(all_targets[[tgt]]$type, "id")) {
      fill_rate <- id_fill_rates[[src]]
      if (!is.null(fill_rate) && fill_rate < ID_MIN_FILL_RATE) {
        message(sprintf("[SUGGEST] Skipping '%s' for ID field '%s': fill rate %.0f%% < %.0f%% minimum",
                        src, tgt, fill_rate * 100, ID_MIN_FILL_RATE * 100))
        next
      }
    }

    # Assign this source to this target with its confidence score
    assignments[[tgt]] <- list(column = src, score = score)
    used_sources <- c(used_sources, src)
  }

  # TIMING: Report performance
  timing_log$total <- as.numeric(difftime(Sys.time(), timing_start, units = "secs"))
  message(sprintf(
    "[TIMING] suggest_all_mappings: %.2fs total (precompute: %.3fs, scoring: %.2fs) | %d cols x %d targets | rapidfuzz: %s",
    timing_log$total, timing_log$precompute, timing_log$scoring,
    length(detected_cols), length(all_targets),
    ifelse(RAPIDFUZZ_AVAILABLE, "YES", "NO")
  ))

  return(assignments)
}

# ============================================================================
# OPTIMIZED COLUMN SCORING - Pre-compute word lists once, vectorize operations
# ============================================================================
# This version is ~10-50x faster for large datasets (300+ columns)
# Key optimizations:
# 1. Pre-compute normalized names and word lists ONCE outside loops
# 2. Use vectorized matching with %in% instead of nested loops
# 3. Skip expensive operations for columns that already have high scores
# ============================================================================

# Pre-compute column metadata for faster scoring (call once per dataset)
precompute_column_metadata <- function(detected_cols) {
  n <- length(detected_cols)

  # Normalized names (lowercase, alphanumeric only)
  col_norm <- tolower(gsub("[^[:alnum:]]", "", detected_cols))

  # Lowercase versions
  col_lower <- tolower(detected_cols)

  # Word lists (split on non-alphanumeric, filter empty)
  col_words <- lapply(detected_cols, function(col) {
    words <- unique(tolower(strsplit(gsub("[^[:alnum:]]", " ", col), "[[:space:]]+")[[1]]))
    words[nzchar(words)]
  })

  # Boolean field detection (ends in YN, Flag, Indicator)
  is_boolean_field <- grepl("(YN|Flag|Indicator)$", detected_cols, ignore.case = TRUE)

  list(
    cols = detected_cols,
    norm = col_norm,
    lower = col_lower,
    words = col_words,
    is_boolean = is_boolean_field
  )
}

# Helper function to calculate scores for one target against all source columns
# OPTIMIZED VERSION - accepts pre-computed column metadata
calculate_column_scores <- function(tgt, meta, detected_cols, dataset = NULL, col_meta = NULL) {
  n_cols <- length(detected_cols)
  scores <- setNames(rep(0, n_cols), detected_cols)

  # Use pre-computed metadata if available, otherwise compute on-the-fly
  if (is.null(col_meta)) {
    col_meta <- precompute_column_metadata(detected_cols)
  }

  dnorm <- col_meta$norm
  col_lower <- col_meta$lower
  col_words <- col_meta$words
  is_boolean <- col_meta$is_boolean

  # Normalize target
  tnorm <- tolower(gsub("[^[:alnum:]]", "", tgt))
  target_type <- meta$type %||% "text"

  # 1. EXACT MATCH (normalized) - Score: 1000 (highest priority)
  exact_idx <- which(dnorm == tnorm)
  if (length(exact_idx)) {
    scores[exact_idx[1]] <- 1000
  }

  # 2. EXACT SYNONYM MATCH - Score: 150 (VECTORIZED)
  # Exact synonym matches should decisively outweigh fuzzy/partial matches (max 80)
  if (length(meta$synonyms)) {
    syn_norm <- tolower(gsub("[^[:alnum:]]", "", meta$synonyms))
    syn_match <- dnorm %in% syn_norm
    scores[syn_match] <- scores[syn_match] + 150
  }

  # 2b. SYNONYM WORD MATCH - Score: 80 (OPTIMIZED with rapidfuzz)
  # Uses rapidfuzz for faster fuzzy matching if available, falls back to R
  if (length(meta$synonyms)) {

    # Try rapidfuzz first (C++ optimized, much faster for large datasets)
    if (RAPIDFUZZ_AVAILABLE) {
      for (syn in meta$synonyms) {
        # Batch match this synonym against all column names
        matches <- batch_fuzzy_match(syn, detected_cols, score_cutoff = 70)
        if (!is.null(matches) && nrow(matches) > 0) {
          for (j in seq_len(nrow(matches))) {
            idx <- matches$index[j]
            if (scores[idx] < 90) {  # Don't override higher scores (exact/synonym exact)
              # Scale rapidfuzz score (0-100) to our scoring system (max 80 for word match)
              # Only add if it's a meaningful improvement
              fuzzy_score <- (matches$score[j] / 100) * 80
              if (fuzzy_score >= 50) {  # Only count reasonably good matches
                scores[idx] <- max(scores[idx], fuzzy_score)
              }
            }
          }
        }
      }
    } else {
      # FALLBACK: Original R logic (slower but works without Python)
      syn_word_lists <- lapply(meta$synonyms, function(syn) {
        words <- unique(tolower(strsplit(gsub("[^[:alnum:]]", " ", syn), "[[:space:]]+")[[1]]))
        words[nzchar(words)]
      })

      for (syn_idx in seq_along(meta$synonyms)) {
        syn_words <- syn_word_lists[[syn_idx]]
        if (length(syn_words) == 0) next

        # Vectorized check: for each column, do all synonym words appear?
        for (i in seq_len(n_cols)) {
          if (scores[i] >= 80) next  # Cap synonym word match at 80 total
          cw <- col_words[[i]]
          if (length(cw) == 0) next

          # Check if all synonym words match (exact or partial)
          all_match <- TRUE
          # Filter column words: only include 3+ char words for reverse matching
          # (prevents short stop words like 'of','at','to','in' from false-matching
          #  inside compound names like 'OffMarketDate' or 'DateOffMarket')
          cw_meaningful <- cw[nchar(cw) >= 3]
          for (sw in syn_words) {
            # Check if synonym word appears in any column word (forward match)
            forward_match <- any(grepl(sw, cw, fixed = TRUE))
            # OR if any meaningful column word appears in synonym word (reverse match)
            # Require the matched word to cover at least 40% of the synonym word
            # to prevent tiny substrings matching large compound words
            reverse_match <- FALSE
            if (!forward_match && length(cw_meaningful) > 0) {
              for (cm in cw_meaningful) {
                if (grepl(cm, sw, fixed = TRUE) && nchar(cm) >= nchar(sw) * 0.4) {
                  reverse_match <- TRUE
                  break
                }
              }
            }
            found <- forward_match || reverse_match
            if (!found) {
              all_match <- FALSE
              break
            }
          }
          if (all_match) {
            scores[i] <- scores[i] + 80
          }
        }
      }
    }
  }

  # 3. PARTIAL WORD MATCH - Score: 50-70
  tgt_words <- unique(tolower(strsplit(gsub("([A-Z])", " \\1", tgt), "[[:space:]]+")[[1]]))
  tgt_words <- tgt_words[nzchar(tgt_words)]
  n_tgt_words <- length(tgt_words)

  if (n_tgt_words > 0) {
    for (i in seq_len(n_cols)) {
      cw <- col_words[[i]]
      n_col_words <- length(cw)
      if (n_col_words == 0) next

      matching <- length(intersect(tgt_words, cw))
      if (matching > 0) {
        word_score <- (matching / max(n_tgt_words, n_col_words)) * 70
        scores[i] <- scores[i] + word_score
      }
    }
  }

  # 4. KEY TERM MATCHING - Score: 40 (VECTORIZED where possible)
  if (target_type == "date" || grepl("date", tolower(tgt))) {
    date_pattern <- "(date|\\bdt\\b|when|time|day)"
    date_match <- grepl(date_pattern, col_lower)
    scores[date_match] <- scores[date_match] + 40

  } else if (target_type == "currency" || grepl("price|cost", tolower(tgt))) {
    price_pattern <- "(price|amount|cost|value|\\bamt\\b)"
    price_match <- grepl(price_pattern, col_lower)
    scores[price_match] <- scores[price_match] + 40

    # Penalize unit-rate patterns
    if (!grepl("ratio|percent|pct", tolower(tgt))) {
      unit_rate <- grepl("(psf|ppsf)\\b", col_lower) |
                   grepl("\\$/\\s*(sf|sq\\s*ft)\\b", col_lower) |
                   (grepl("(per|/)", col_lower) &
                    grepl("(sq\\s*ft|sqft|sf|square|acre|ac|unit|month|mo|year|yr)\\b", col_lower))
      scores[unit_rate] <- scores[unit_rate] - 120

      # Penalize ratio columns
      ratio_cols <- vapply(detected_cols, is_price_ratio_column_name, logical(1))
      scores[ratio_cols] <- scores[ratio_cols] - 140
    }

    # StrataFees-specific
    if (identical(tolower(tgt), "stratafees")) {
      fee_terms <- grepl("(fee|fees|dues|hoa|strata|condo|maintenance|assoc)", col_lower)
      scores[fee_terms] <- scores[fee_terms] + 35

      concession_terms <- grepl("(concession|credit|rebate|allowance|sellerpaid|closingcost)", col_lower)
      scores[concession_terms] <- scores[concession_terms] - 120
    }

  } else if (target_type == "id" || grepl("\\bid\\b", tolower(tgt))) {
    id_pattern <- "(\\bid\\b|number|\\bnum\\b|\\bnbr\\b|\\bno\\b)"
    id_match <- grepl(id_pattern, col_lower)
    scores[id_match] <- scores[id_match] + 40
  }

  # 5. DATA CONTENT INSPECTION - Score: 30 (OPTIMIZED - sample once per column)
  if (!is.null(dataset)) {
    cols_in_data <- detected_cols %in% names(dataset)

    for (i in which(cols_in_data)) {
      col_name <- detected_cols[i]
      sample_vals <- head(dataset[[col_name]], 20)
      sample_vals <- sample_vals[!is.na(sample_vals) & nzchar(as.character(sample_vals))]

      if (length(sample_vals) == 0) next

      if (target_type == "date" || grepl("date", tolower(tgt))) {
        if (any(grepl("[0-9]{4}|[0-9]{1,2}[/-][0-9]{1,2}", sample_vals))) {
          scores[i] <- scores[i] + 30
        }
      } else if (target_type == "currency" || grepl("price", tolower(tgt))) {
        numeric_count <- sum(grepl("^[\\$0-9,\\.]+$", sample_vals))
        if (numeric_count / length(sample_vals) > 0.5) {
          scores[i] <- scores[i] + 30
        }
        # Penalize ratio-like values
        numeric_vals <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", sample_vals)))
        numeric_vals <- numeric_vals[!is.na(numeric_vals)]
        if (length(numeric_vals) >= 5 && mean(numeric_vals >= 0 & numeric_vals <= 5) > 0.6) {
          scores[i] <- scores[i] - 120
        }
      } else if (target_type == "number") {
        numeric_vals <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", sample_vals)))
        if (sum(!is.na(numeric_vals)) / length(sample_vals) > 0.5) {
          scores[i] <- scores[i] + 30
        }
      } else if (target_type == "text") {
        has_letters <- mean(grepl("[A-Za-z]", sample_vals))
        mostly_numeric <- mean(grepl("^[[:space:]]*#?[0-9]+$", sample_vals))
        if (!is.nan(has_letters) && has_letters > 0.5) scores[i] <- scores[i] + 10
        if (!is.nan(mostly_numeric) && mostly_numeric > 0.6) scores[i] <- scores[i] - 40
      }
    }

    # Penalize empty columns (VECTORIZED)
    # Don't zero out columns with synonym exact matches (score >= 90) —
    # the name match is strong enough to trust even with sparse data
    for (i in which(cols_in_data & scores > 0)) {
      col_values <- dataset[[detected_cols[i]]]
      non_empty <- sum(!is.na(col_values) & nzchar(trimws(as.character(col_values))))
      if (non_empty < (nrow(dataset) * 0.05) && scores[i] < 90) {
        scores[i] <- 0
      }
    }
  }

  # 6. YN SUFFIX PENALTY (VECTORIZED)
  if (target_type != "boolean") {
    scores[is_boolean] <- scores[is_boolean] - 500
  }

  # 7. SEMANTIC MISMATCH PENALTIES - Prevent known bad partial matches
  # These penalties address cases where partial word matching creates false positives

  # 7a. Square feet/area targets should NOT match dimension columns (text like "100x200")
  if (grepl("(sqft|squarefeet|area|sqm|sf)$", tnorm, ignore.case = TRUE)) {
    dimension_cols <- grepl("dimension", col_lower, ignore.case = TRUE)
    scores[dimension_cols] <- scores[dimension_cols] - 500
  }

  # 7b. Bath count targets (Full/Half/Total) should NOT match location-based bath columns,
  #     non-bath columns like SF/area, or generic room columns without "bath"
  if (grepl("^(ba|pb|bathroomsfull|bathroomshalf|bathroomstotalinteger)$", tnorm, ignore.case = TRUE)) {
    # Penalize SF/area columns
    sf_area_cols <- grepl("(\\bsf\\b|sqft|squarefeet|area)", col_lower, ignore.case = TRUE)
    scores[sf_area_cols] <- scores[sf_area_cols] - 500

    # Penalize location-based bath columns (upper/lower/main/master)
    location_bath_cols <- grepl("(upper|lower|main|master|primary|basement|ground|first|second).*bath", col_lower, ignore.case = TRUE)
    scores[location_bath_cols] <- scores[location_bath_cols] - 500

    # Penalize generic "room" columns that don't contain "bath"
    # This prevents "Add room 1 desc" from matching BathroomsFull
    room_not_bath <- grepl("room", col_lower, ignore.case = TRUE) & !grepl("bath", col_lower, ignore.case = TRUE)
    scores[room_not_bath] <- scores[room_not_bath] - 500
  }

  # 7c. Area/SF targets should NOT match bath columns
  if (grepl("(area|sqft|sf|squarefeet|livingarea|buildingarea)$", tnorm, ignore.case = TRUE)) {
    bath_cols <- grepl("bath", col_lower, ignore.case = TRUE)
    scores[bath_cols] <- scores[bath_cols] - 500
  }

  return(scores)
}

address_missing_street_number <- function(values, min_digit_ratio = 0.55, sample_size = 150) {
  vals <- trimws(as.character(values))
  vals <- vals[nzchar(vals)]
  if (!length(vals)) {
    return(TRUE)
  }
  sample_vals <- head(vals, sample_size)
  digit_ratio <- mean(grepl("^[[:space:]]*#?[0-9]", sample_vals))
  if (is.nan(digit_ratio)) digit_ratio <- 0
  digit_ratio < min_digit_ratio
}

find_street_number_column <- function(dataset, exclude_cols = character()) {
  candidates <- setdiff(names(dataset), exclude_cols)
  if (!length(candidates)) return(NULL)

  pattern_preferred <- "(^|_|\\b)(street|st|str|addr|address|house|property)(number|num|no|nbr)"
  pattern_exclude <- "(zip|postal|mls|pid|parcel|id|unit|suite|apt|lot)"

  best_col <- NULL
  best_score <- 0

  for (col in candidates) {
    col_lower <- tolower(col)
    if (grepl(pattern_exclude, col_lower)) next

    values <- dataset[[col]]
    if (all(is.na(values))) next

    if (is.numeric(values)) {
      vals_chr <- trimws(as.character(values))
    } else {
      vals_chr <- trimws(as.character(values))
    }
    vals_chr <- vals_chr[nzchar(vals_chr)]
    if (!length(vals_chr)) next

    sample_vals <- head(vals_chr, 200)
    number_like <- grepl("^[[:space:]]*#?[0-9]{1,6}([A-Za-z]|[/-][0-9]{1,4}|[[:space:]]?[A-Za-z0-9]{1,4})?$", sample_vals)
    numeric_ratio <- mean(number_like)
    if (is.nan(numeric_ratio)) next

    if (numeric_ratio < 0.15) next

    name_bonus <- if (grepl(pattern_preferred, col_lower)) 0.35 else 0
    numeric_bonus <- if (is.numeric(values)) 0.15 else 0
    score <- numeric_ratio + name_bonus + numeric_bonus

    if (score > best_score) {
      best_score <- score
      best_col <- col
    }
  }

  if (!is.null(best_col) && best_score >= 0.3) {
    list(column = best_col, score = best_score)
  } else {
    NULL
  }
}

# Heuristic: does a column look like mostly street names (not numbers)?
address_looks_like_street_name <- function(values, min_name_ratio = 0.5, sample_size = 200) {
  vals <- trimws(as.character(values))
  vals <- vals[nzchar(vals)]
  if (!length(vals)) return(FALSE)
  smp <- head(vals, sample_size)
  # starts with non-digit and contains letters
  name_like <- grepl("^[^0-9]", smp) & grepl("[A-Za-z]", smp)
  mean(name_like, na.rm = TRUE) >= min_name_ratio
}

find_street_name_column <- function(dataset, exclude_cols = character()) {
  candidates <- setdiff(names(dataset), exclude_cols)
  if (!length(candidates)) return(NULL)

  pattern_preferred <- paste(
    c("street", "st", "str", "avenue", "ave", "av", "road", "rd", "drive", "dr",
      "lane", "ln", "boulevard", "blvd", "court", "ct", "circle", "cir", "way",
      "trail", "trl", "terrace", "ter", "place", "pl", "highway", "hwy", "route", "rte",
      "streetname", "stname", "addr", "address", "street_name"),
    collapse = "|"
  )
  pattern_exclude <- "(zip|postal|mls|pid|parcel|id|unit|suite|apt|lot|city|town|state|province|country|postalcode|terms|listing|remarks|comment|condition|financ|status|contract|agreement|offer|conting)"

  best_col <- NULL
  best_score <- 0

  for (col in candidates) {
    col_lower <- tolower(col)
    if (grepl(pattern_exclude, col_lower)) next
    values <- dataset[[col]]
    if (all(is.na(values))) next
    vals_chr <- trimws(as.character(values))
    vals_chr <- vals_chr[nzchar(vals_chr)]
    if (!length(vals_chr)) next

    # Compute heuristics
    smp <- head(vals_chr, 200)
    starts_with_digit_ratio <- mean(grepl("^[0-9]", smp))
    has_alpha_ratio <- mean(grepl("[A-Za-z]", smp))
    space_ratio <- mean(grepl("\x20", smp))
    suffix_ratio <- mean(grepl(paste0("\\b(", pattern_preferred, ")\\b"), tolower(smp)))

    # prefer not starting with digits, with letters, often spaces, and suffix tokens
    score <- (1 - starts_with_digit_ratio) * 0.4 + has_alpha_ratio * 0.2 + space_ratio * 0.15 + suffix_ratio * 0.25
    # name bonus by column name
    # add boundaries to avoid matching 'st' inside 'listing'
    name_bonus <- if (grepl(paste0("(^|\\b|_)(", pattern_preferred, ")($|\\b|_)"), col_lower)) 0.2 else 0
    score <- score + name_bonus

    if (is.finite(score) && score > best_score) {
      best_score <- score
      best_col <- col
    }
  }

  if (!is.null(best_col) && best_score >= 0.5) list(column = best_col, score = best_score) else NULL
}

# ============================================================================
# COLUMN ANALYSIS MODULE - Data Quality Metrics
# ============================================================================
# Calculates data quality metrics for column classification:
# - Fill rate (non-empty values as percentage)
# - Unique value count
# - Data type detection (text, number, date, boolean, mixed)
# - Quality flags (VERY_LOW_FILL, LOW_FILL, NO_VARIANCE, CORRUPT_MIXED)
#
# Reference: docs/column_classification_logic.md
# ============================================================================

# Thresholds for quality flags and classification
FILL_THRESHOLD_EMPTY <- 0.00      # 0% = completely empty → Class 4
FILL_THRESHOLD_VERY_LOW <- 0.05   # < 5% = VERY_LOW_FILL → Class 3
FILL_THRESHOLD_LOW <- 0.20        # < 20% = LOW_FILL (informational)
MIN_ROWS_FOR_VARIANCE <- 10       # Only flag NO_VARIANCE if dataset has > 10 rows

# Classification Upgrade Workflow Concept:
# -----------------------------------------
# Empty/low-fill columns are auto-classified as Class 4 (exclude) to keep analysis clean.
# When a user loads a saved profile with new source data:
#   - If previously Class 4 columns now have data, they can be flagged for review
#   - User can upgrade classification (4 → 3 → 2 → 1) and update/save new profile
#   - Supports different market types (e.g., same MLS but different search criteria)
#   - Example: Basement finish data may be empty for condos but populated for SFR
#
# Future: "Common Data Logic" per property type could alert users:
#   "For single-family residential, these variables are typically populated: [list]"
#   This could be MLS-specific or based on shared market logic patterns.

#' Analyze a single column and return data quality metrics
#'
#' @param values Vector of values from one column
#' @param col_name Name of the column (for pattern-based detection)
#' @param total_rows Total rows in dataset (for fill rate calculation)
#' @return Named list with: fill_rate, fill_count, unique_count, inferred_type, flags, sample_values
analyze_column <- function(values, col_name, total_rows = length(values)) {
  # Handle NULL/empty input
  if (is.null(values) || length(values) == 0) {
    return(list(
      fill_rate = 0,
      fill_count = 0,
      unique_count = 0,
      inferred_type = "empty",
      flags = c("EMPTY_COLUMN"),
      sample_values = character(0)
    ))
  }

  # Calculate fill rate (non-NA, non-empty values)
  vals_chr <- trimws(as.character(values))
  non_empty <- !is.na(values) & nzchar(vals_chr)
  fill_count <- sum(non_empty)
  fill_rate <- fill_count / total_rows

  # Get non-empty values for analysis
  filled_vals <- vals_chr[non_empty]

  # Unique value count
  unique_count <- length(unique(filled_vals))

  # Sample values (first 5 non-empty unique values)
  sample_values <- head(unique(filled_vals), 5)

  # Infer data type
  inferred_type <- infer_column_type(filled_vals, col_name)

  # Quality flags
  flags <- character(0)

  if (fill_rate < FILL_THRESHOLD_VERY_LOW) {
    flags <- c(flags, "VERY_LOW_FILL")
  } else if (fill_rate < FILL_THRESHOLD_LOW) {
    flags <- c(flags, "LOW_FILL")
  }

  if (unique_count == 1 && total_rows > MIN_ROWS_FOR_VARIANCE) {
    flags <- c(flags, "NO_VARIANCE")
  }

  if (inferred_type == "mixed") {
    flags <- c(flags, "CORRUPT_MIXED")
  }

  list(
    fill_rate = fill_rate,
    fill_count = fill_count,
    unique_count = unique_count,
    inferred_type = inferred_type,
    flags = flags,
    sample_values = sample_values
  )
}

#' Infer data type from column values
#'
#' @param values Character vector of non-empty values
#' @param col_name Column name for pattern hints
#' @return String: "text", "number", "currency", "date", "boolean", "phone", "id", "mixed"
infer_column_type <- function(values, col_name = "") {
  if (length(values) == 0) return("empty")

  # Sample for efficiency (analyze first 200 values)
  sample_vals <- head(values, 200)
  n <- length(sample_vals)

  col_lower <- tolower(col_name)

  # Count matches for each type
  type_counts <- list(
    number = 0,
    currency = 0,
    date = 0,
    boolean = 0,
    phone = 0,
    id = 0,
    text = 0
  )

  # Patterns for type detection
  pattern_number <- "^-?[0-9]+\\.?[0-9]*$"
  pattern_currency <- "^\\$?-?[0-9,]+\\.?[0-9]*$"
  pattern_date <- "([0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2})|([0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{2,4})"
  pattern_boolean <- "^(Y|N|Yes|No|TRUE|FALSE|True|False|true|false|1|0)$"
  pattern_phone <- "^\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4}$"
  pattern_id <- "^[A-Z]{0,3}[0-9]{5,15}$"

  for (val in sample_vals) {
    if (grepl(pattern_boolean, val)) {
      type_counts$boolean <- type_counts$boolean + 1
    } else if (grepl(pattern_date, val)) {
      type_counts$date <- type_counts$date + 1
    } else if (grepl(pattern_phone, val)) {
      type_counts$phone <- type_counts$phone + 1
    } else if (grepl(pattern_currency, val) && grepl("\\$|,", val)) {
      type_counts$currency <- type_counts$currency + 1
    } else if (grepl(pattern_number, val)) {
      type_counts$number <- type_counts$number + 1
    } else if (grepl(pattern_id, val, ignore.case = TRUE)) {
      type_counts$id <- type_counts$id + 1
    } else {
      type_counts$text <- type_counts$text + 1
    }
  }

  # Determine dominant type (>50% of values)
  type_ratios <- sapply(type_counts, function(x) x / n)
  max_ratio <- max(type_ratios)
  dominant_type <- names(type_counts)[which.max(type_ratios)]

  # Check for mixed types (no clear majority, multiple significant types)
  significant_types <- sum(type_ratios > 0.15)
  if (max_ratio < 0.5 && significant_types >= 3) {
    return("mixed")
  }

  # Use column name hints to refine type
  if (grepl("(date|dt$|_dt$)", col_lower) && type_counts$date > 0) {
    return("date")
  }
  if (grepl("(price|cost|amount|fee)", col_lower) && (type_counts$currency > 0 || type_counts$number > 0)) {
    return("currency")
  }
  if (grepl("(phone|mobile|cell|fax|tel)", col_lower)) {
    return("phone")
  }
  if (grepl("(yn$|_yn$|flag|indicator)", col_lower)) {
    return("boolean")
  }
  if (grepl("(id$|_id$|key|number|no$|num$)", col_lower) && type_counts$number > type_counts$text) {
    return("id")
  }

  return(dominant_type)
}

#' Analyze all columns in a dataset
#'
#' @param dataset Data frame to analyze
#' @return Data frame with one row per column containing metrics
analyze_all_columns <- function(dataset) {
  if (is.null(dataset) || ncol(dataset) == 0) {
    return(data.frame(
      column_name = character(0),
      fill_rate = numeric(0),
      fill_count = integer(0),
      unique_count = integer(0),
      inferred_type = character(0),
      flags = character(0),
      sample_values = character(0),
      stringsAsFactors = FALSE
    ))
  }

  total_rows <- nrow(dataset)
  col_names <- names(dataset)

  # Analyze each column
  results <- lapply(col_names, function(col) {
    analysis <- analyze_column(dataset[[col]], col, total_rows)
    data.frame(
      column_name = col,
      fill_rate = analysis$fill_rate,
      fill_count = analysis$fill_count,
      unique_count = analysis$unique_count,
      inferred_type = analysis$inferred_type,
      flags = paste(analysis$flags, collapse = ", "),
      sample_values = paste(analysis$sample_values, collapse = " | "),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

#' Detect duplicate columns (same name and/or same values)
#'
#' @param dataset Data frame to analyze
#' @return Data frame with duplicate groups
detect_duplicate_columns <- function(dataset) {
  if (is.null(dataset) || ncol(dataset) < 2) {
    return(data.frame(
      column_name = character(0),
      duplicate_of = character(0),
      match_type = character(0),
      stringsAsFactors = FALSE
    ))
  }

  col_names <- names(dataset)
  n_cols <- length(col_names)

  duplicates <- list()

  # Check for name duplicates first
  name_counts <- table(col_names)
  dup_names <- names(name_counts)[name_counts > 1]

  for (dup_name in dup_names) {
    dup_indices <- which(col_names == dup_name)
    first_idx <- dup_indices[1]

    for (idx in dup_indices[-1]) {
      # Check if values also match
      vals_match <- identical(dataset[[first_idx]], dataset[[idx]])
      match_type <- if (vals_match) "name_and_values" else "name_only"

      duplicates[[length(duplicates) + 1]] <- data.frame(
        column_name = col_names[idx],
        column_index = idx,
        duplicate_of = col_names[first_idx],
        duplicate_of_index = first_idx,
        match_type = match_type,
        stringsAsFactors = FALSE
      )
    }
  }

  # Check for value-only duplicates (different names, same values)
  # This is expensive, so only check columns with same data types
  checked_pairs <- character(0)

  for (i in 1:(n_cols - 1)) {
    for (j in (i + 1):n_cols) {
      # Skip if already identified as name duplicate
      pair_key <- paste(i, j, sep = "-")
      if (pair_key %in% checked_pairs) next
      if (col_names[i] == col_names[j]) next

      # Quick type check first
      if (class(dataset[[i]])[1] != class(dataset[[j]])[1]) next

      # Check if values are identical
      if (identical(dataset[[i]], dataset[[j]])) {
        duplicates[[length(duplicates) + 1]] <- data.frame(
          column_name = col_names[j],
          column_index = j,
          duplicate_of = col_names[i],
          duplicate_of_index = i,
          match_type = "values_only",
          stringsAsFactors = FALSE
        )
        checked_pairs <- c(checked_pairs, pair_key)
      }
    }
  }

  if (length(duplicates) == 0) {
    return(data.frame(
      column_name = character(0),
      column_index = integer(0),
      duplicate_of = character(0),
      duplicate_of_index = integer(0),
      match_type = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, duplicates)
}

#' Format fill rate as percentage string with color coding
#'
#' @param fill_rate Numeric fill rate (0-1)
#' @return Character string formatted as percentage
format_fill_rate <- function(fill_rate) {
  pct <- round(fill_rate * 100, 1)
  sprintf("%.1f%%", pct)
}

#' Get quality status based on flags
#'
#' @param flags Character vector of quality flags
#' @return String: "good", "warning", "error"
get_quality_status <- function(flags) {
  if (length(flags) == 0 || all(flags == "")) return("good")
  if ("CORRUPT_MIXED" %in% flags || "VERY_LOW_FILL" %in% flags) return("error")
  if ("LOW_FILL" %in% flags || "NO_VARIANCE" %in% flags) return("warning")
  return("good")
}

# ============================================================================
# COLUMN CLASSIFICATION MODULE - Pattern-Based Auto-Classification
# ============================================================================
# Implements classification rules from docs/column_classification_logic.md
#
# Classes:
#   1 = Core Analytical (analyst-facing, directly used in analysis)
#   2 = Supplemental (app-facing, background calculations, reporting)
#   3 = Remove (no analytical value)
#
# Rules implemented:
#   Rule 5: Automatic Class 3 patterns (KeyNumeric, AOR, obsolete contacts, etc.)
#   Rule 6: Geographic fields classification
#   Rule 4: Date/Timestamp field classification
# ============================================================================

# Class 3 patterns - Marginal fields (might be useful someday)
CLASS_3_PATTERNS <- list(
  # Phone extensions (low value but not PII)
  phone_ext = list(
    pattern = "(PhoneExt|Ext$)",
    reason = "Phone extension (low value)"
  ),

  # Commercial-focused (rare in residential)
  land_lease = list(
    pattern = "^LandLease",
    reason = "Land lease fields (commercial-focused)"
  ),

  # Marketing/operational fields (non-PII)
  virtual_tour = list(
    pattern = "^VirtualTour(?!.*URL)",
    reason = "Marketing field (virtual tour)"
  ),
  photo_meta = list(
    pattern = "^Photo(Count|sCount|sChangeTimestamp|Instructions)",
    reason = "Photo metadata"
  ),
  showing = list(
    pattern = "^Showing(Instructions|Attended)",
    reason = "Showing logistics (operational)"
  ),
  contact_prefs = list(
    pattern = "^Contact(Preference|Type|Method)",
    reason = "MLS contact preferences"
  ),

  # Low-value descriptive fields
  open_house_count = list(
    pattern = "^OpenHouseCount$",
    reason = "Open house count (low analytical value)"
  ),
  possession = list(
    pattern = "^Possession$",
    reason = "Transaction detail (not valuation)"
  ),
  eating_area = list(
    pattern = "^EatingArea$",
    reason = "Low analytical value"
  ),
  door_features = list(
    pattern = "^DoorFeatures$",
    reason = "Low analytical value"
  )
  # NOTE: agent_name and office_name moved to CLASS_4_PATTERNS (PII)
)

# Class 4 patterns - PII, contact info, internal IDs - NEVER export
CLASS_4_PATTERNS <- list(
  # Agent/Broker contact information (PII)
  agent_email = list(
    pattern = "(Agent|Broker|Office).*Email",
    reason = "Agent/broker email (PII)"
  ),
  agent_phone = list(
    pattern = "(Agent|Broker).*(Phone|DirectPhone|HomePhone|CellPhone|MobilePhone)$",
    reason = "Agent/broker phone (PII)"
  ),
  agent_phone_prefix = list(
    pattern = "(Agent|Broker)Phone",
    reason = "Agent/broker phone (PII)"
  ),
  agent_cell = list(
    pattern = "(Agent|Broker).*(Cell|Mobile)",
    reason = "Agent/broker mobile (PII)"
  ),
  agent_fax = list(
    pattern = "(Agent|Broker|Office).*Fax",
    reason = "Agent/broker fax (PII)"
  ),
  agent_url = list(
    pattern = "(Agent|Broker).*URL",
    reason = "Agent/broker website (PII)"
  ),

  # Owner/Seller information (PII)
  owner_name = list(
    pattern = "(Owner|Seller).*Name",
    reason = "Owner/seller name (PII)"
  ),
  owner_phone = list(
    pattern = "(Owner|Seller).*Phone",
    reason = "Owner/seller phone (PII)"
  ),

  # Internal MLS keys - should never be exported
  listing_key = list(
    pattern = "^ListingKey$",
    reason = "Internal MLS listing key"
  ),
  key_numeric = list(
    pattern = "KeyNumeric$",
    reason = "Internal MLS key (numeric)"
  ),
  aor_code = list(
    pattern = "AOR$|^AOR",
    reason = "Association of Realtors internal code"
  ),

  # Agent/Broker internal IDs
  agent_key = list(
    pattern = "(Agent|Broker)Key$",
    reason = "Internal agent/broker key"
  ),
  office_key = list(
    pattern = "Office.*Key$",
    reason = "Internal office key"
  ),
  member_key = list(
    pattern = "Member.*Key$",
    reason = "Internal member key"
  ),

  # Obsolete contact methods (PII adjacent)
  voicemail = list(
    pattern = "Voicemail",
    reason = "Obsolete contact method"
  ),
  pager = list(
    pattern = "Pager",
    reason = "Obsolete contact method"
  ),
  toll_free = list(
    pattern = "TollFree",
    reason = "Toll-free number (operational)"
  ),

  # Co-agent contact info
  co_agent_contact = list(
    pattern = "^Co(List|Buyer).*(Email|Phone|Cell|Mobile|Fax)",
    reason = "Co-agent contact info (PII)"
  ),

  # MLS internal operational
  deleted_flag = list(
    pattern = "^Deleted(YN)?$",
    reason = "MLS internal deletion flag"
  ),
  auto_sold = list(
    pattern = "^AutoSold",
    reason = "MLS auto-sold flag"
  ),

  # Syndication/Marketing operational
  internet_display = list(
    pattern = "Internet.*Display",
    reason = "Syndication display setting"
  ),
  virtual_tour_url = list(
    pattern = "^VirtualTour.*URL",
    reason = "Marketing URL"
  ),

  # ============================================================
  # AGGRESSIVE AGENT/OFFICE CATCH-ALL PATTERNS

  # Catch ALL fields starting with these prefixes (except Keys)
  # User can reclassify false positives and save to profile
  # ============================================================

  # Agent Names (moved from Class 3 - treating as PII)
  agent_name = list(
    pattern = "(List|Buyer|Selling|Co.*)?Agent.*Name",
    reason = "Agent name (PII)"
  ),
  broker_name = list(
    pattern = "Broker.*Name",
    reason = "Broker name (PII)"
  ),

  # Office Names and Contact (expanded)
  office_name = list(
    pattern = "^(List|Co.*)?Office.*Name",
    reason = "Office name (PII)"
  ),
  office_phone = list(
    pattern = "^Office.*(Phone|Mobile)",
    reason = "Office phone (PII)"
  ),
  office_url = list(
    pattern = "^Office.*(URL|Website|Web)",
    reason = "Office website (PII)"
  ),

  # Team/Group fields
  team_contact = list(
    pattern = "^Team.*(Name|Email|Phone|Cell|URL)",
    reason = "Team contact info (PII)"
  ),
  group_contact = list(
    pattern = "^Group.*(Name|Email|Phone|Cell|URL)",
    reason = "Group contact info (PII)"
  ),

  # Assistant fields
  assistant_contact = list(
    pattern = "^Assistant.*(Email|Phone|Cell|Mobile)",
    reason = "Assistant contact (PII)"
  ),

  # Owner address (separate from property address)
  owner_address = list(
    pattern = "^Owner.*(Address|City|State|Postal|Zip)",
    reason = "Owner address (PII)"
  ),

  # Aggressive catch-all for ListAgent* fields (except Keys)
  list_agent_all = list(
    pattern = "^ListAgent(?!.*Key$).*",
    reason = "Listing agent field (PII)"
  ),
  buyer_agent_all = list(
    pattern = "^BuyerAgent(?!.*Key$).*",
    reason = "Buyer agent field (PII)"
  ),
  selling_agent_all = list(
    pattern = "^SellingAgent(?!.*Key$).*",
    reason = "Selling agent field (PII)"
  ),
  co_list_agent_all = list(
    pattern = "^CoListAgent(?!.*Key$).*",
    reason = "Co-listing agent field (PII)"
  ),
  co_buyer_agent_all = list(
    pattern = "^CoBuyerAgent(?!.*Key$).*",
    reason = "Co-buyer agent field (PII)"
  ),

  # Catch-all for ALL CoList columns (entire section typically excluded)
  co_list_all = list(
    pattern = "^CoList",
    reason = "Co-listing data (typically excluded)"
  ),
  # Catch-all for ALL CoBuyer columns
  co_buyer_all = list(
    pattern = "^CoBuyer",
    reason = "Co-buyer data (typically excluded)"
  ),

  # Contact order fields (internal system data)
  contact_order = list(
    pattern = "ContactOrder",
    reason = "Contact order (internal system data)"
  ),

  # LockBox fields (security/internal)
  lockbox_all = list(
    pattern = "^LockBox",
    reason = "LockBox data (security/internal)"
  ),

  # Internet display flags (internal)
  internet_address_display = list(
    pattern = "InternetAddressDisplay",
    reason = "Internet display flag (internal)"
  ),
  internet_entire_listing = list(
    pattern = "InternetEntireListingDisp",
    reason = "Internet display flag (internal)"
  ),

  # Association management (PII)
  association_mgmt_name = list(
    pattern = "AssociationManagementName",
    reason = "Association management name (PII)"
  ),
  association_phone = list(
    pattern = "AssociationPhone",
    reason = "Association phone (PII)"
  ),

  # BuyerAgent catch-all (PII)
  buyer_agent_all = list(
    pattern = "^BuyerAgent",
    reason = "Buyer agent data (PII)"
  ),

  # OtherPhone fields (PII)
  other_phone_all = list(
    pattern = "^OtherPhone",
    reason = "Other phone numbers (PII)"
  ),

  # Owner name (PII)
  owner_name = list(
    pattern = "OwnerName",
    reason = "Owner name (PII)"
  ),

  # Virtual tour URLs (marketing/internal)
  virtual_tour = list(
    pattern = "VirtualTour",
    reason = "Virtual tour URL (marketing/internal)"
  ),

  # Documents metadata (internal)
  documents_change_timestamp = list(
    pattern = "DocumentsChangeTimestamp",
    reason = "Documents metadata (internal)"
  ),
  documents_count = list(
    pattern = "DocumentsCount",
    reason = "Documents metadata (internal)"
  ),

  # Showing contact/instructions (PII/internal)
  showing_contact_all = list(
    pattern = "^ShowingContact",
    reason = "Showing contact info (PII)"
  ),
  showing_instructions = list(
    pattern = "ShowingInstructions",
    reason = "Showing instructions (internal)"
  ),

  # Aggressive catch-all for Office* fields (except Keys)
  list_office_all = list(
    pattern = "^ListOffice(?!.*Key$).*",
    reason = "Listing office field (PII)"
  ),
  buyer_office_all = list(
    pattern = "^BuyerOffice(?!.*Key$).*",
    reason = "Buyer office field (PII)"
  ),

  # ============================================================
  # MLS INTERNAL AGENT/BROKER CODES
  # These are person-identifiers, not property/listing identifiers
  # ============================================================

  # Agent/Broker ID codes (person identifiers, not unique per listing)
  said_code = list(
    pattern = "^SAID$|^CoSAID$",
    reason = "Selling Agent ID code (internal)"
  ),
  baid_code = list(
    pattern = "^BAID$",
    reason = "Buyer Agent ID code (internal)"
  ),
  brcd_code = list(
    pattern = "^BRCD$|^CoBRCD$",
    reason = "Broker code (internal)"
  ),

  # Generic agent/office shorthand fields (no context, just raw names)
  agent_shorthand = list(
    pattern = "^Agent$|^CoAgent$|^B/Agt$",
    reason = "Agent name shorthand (PII)"
  ),
  office_shorthand = list(
    pattern = "^Office$|^B/Office$",
    reason = "Office name shorthand (PII)"
  ),

  # Phone/Contact shorthand
  fax_shorthand = list(
    pattern = "^Fax$",
    reason = "Fax number (PII)"
  ),
  cell_shorthand = list(
    pattern = "^Cell(/Text)?$|^CoPhone$",
    reason = "Cell/mobile number (PII)"
  ),
  phone_ext = list(
    pattern = "Phone.*Ext",
    reason = "Phone extension (PII)"
  ),

  # Showing/Lockbox operational
  show_hours = list(
    pattern = "^Show.*Hours|^LBHrs",
    reason = "Showing hours/lockbox info (operational)"
  ),

  # Owner/Contact fields
  owner_field = list(
    pattern = "^Owner$|^Owner Name",
    reason = "Owner name (PII)"
  ),
  contact_fields = list(
    pattern = "^1st Contact$|^2nd Contact$|^Tenant/Other$",
    reason = "Contact person (PII)"
  )
)

# Class 2 patterns - supplemental fields (Rule 6 geographic, Rule 4 timestamps)
CLASS_2_PATTERNS <- list(
  # Geographic - used programmatically
  latitude = list(
    pattern = "^Latitude$",
    reason = "Geographic coordinate (programmatic use)"
  ),
  longitude = list(
    pattern = "^Longitude$",
    reason = "Geographic coordinate (programmatic use)"
  ),
  county = list(
    pattern = "(County|Parish)$",
    reason = "County/Parish (may be needed for cross-county analysis)"
  ),
  subdivision = list(
    pattern = "^Subdivision(Name)?$",
    reason = "Subdivision (often low fill, supplemental)"
  ),


  # Timestamps for background calculations (Rule 4)
  modification_timestamp = list(
    pattern = "ModificationTimestamp",
    reason = "App-level timestamp for calculations"
  ),
  status_change_timestamp = list(
    pattern = "StatusChangeTimestamp",
    reason = "App-level timestamp for calculations"
  ),
  price_change_timestamp = list(
    pattern = "PriceChangeTimestamp",
    reason = "App-level timestamp for calculations"
  ),
  contract_status_change = list(
    pattern = "^ContractStatusChangeDate$",
    reason = "App-level date for calculations"
  ),

  # Source metadata
  source_fields = list(
    pattern = "(Source$|^.*Source$)",
    reason = "Source metadata (not primary data)"
  ),

  # Supplemental features
  fireplace_features = list(
    pattern = "^FireplaceFeatures$",
    reason = "Supplemental to FireplaceYN"
  ),
  laundry = list(
    pattern = "^Laundry(Features|YN)$",
    reason = "Supplemental feature"
  ),
  heating_yn = list(
    pattern = "^HeatingYN$",
    reason = "Supplemental to Heating field"
  ),
  appliances = list(
    pattern = "^Appliances(YN)?$",
    reason = "Supplemental feature"
  ),
  patio_porch = list(
    pattern = "^PatioAndPorchFeatures$",
    reason = "Supplemental exterior feature"
  ),
  fencing = list(
    pattern = "^(Fencing|FenceYN)$",
    reason = "Supplemental exterior feature"
  ),
  spa = list(
    pattern = "^Spa(Features|YN)$",
    reason = "Supplemental to Pool fields"
  ),
  flooring = list(
    pattern = "^Flooring$",
    reason = "Supplemental interior feature"
  ),
  parking_features = list(
    pattern = "^Parking(YN|Features)$",
    reason = "Supplemental to ParkingTotal/GarageSpaces"
  )
)

# Class 1 patterns - core analytical fields
CLASS_1_PATTERNS <- list(
  # Core location (Rule 6)
  city = list(
    pattern = "^City$",
    reason = "Core location field"
  ),
  postal_code = list(
    pattern = "^PostalCode(Plus4)?$",
    reason = "Essential location field"
  ),
  mls_area = list(
    pattern = "^MLSArea(Major|Minor)$",
    reason = "MLS geographic area"
  ),

  # Core dates (Rule 4)
  close_date = list(
    pattern = "^(CloseDate|ClosingDate|DateSold)$",
    reason = "Core transaction date"
  ),
  list_date = list(
    pattern = "^(ListingContractDate|ListDate|DateListed)$",
    reason = "Core listing date"
  ),
  on_market_date = list(
    pattern = "^OnMarketDate$",
    reason = "Core market date"
  ),
  dom = list(
    pattern = "^(DaysOnMarket|DOM|CumulativeDaysOnMarket|CDOM)$",
    reason = "Core market metric"
  ),

  # Core property characteristics
  bedrooms = list(
    pattern = "^(Bedrooms|BedroomsTotal|Beds)$",
    reason = "Core property attribute"
  ),
  bathrooms_full = list(
    pattern = "^(BathroomsFull|BathroomsFullAndThreeQuarter)$",
    reason = "Core property attribute"
  ),
  bathrooms_total = list(
    pattern = "^Bathrooms(Total|TotalInteger)$",
    reason = "Core property attribute"
  ),
  living_area = list(
    pattern = "^(LivingArea|LivingAreaSquareFeet|SqFt|SquareFeet|GLA)$",
    reason = "Core size metric"
  ),
  lot_size = list(
    pattern = "^(LotSize|LotSizeSquareFeet|LotSizeAcres|LotArea)$",
    reason = "Core lot metric"
  ),
  year_built = list(
    pattern = "^YearBuilt$",
    reason = "Core property attribute"
  ),

  # Core pricing
  list_price = list(
    pattern = "^(ListPrice|OriginalListPrice)$",
    reason = "Core pricing field"
  ),
  close_price = list(
    pattern = "^(ClosePrice|SoldPrice|SalePrice)$",
    reason = "Core pricing field"
  ),

  # Core identifiers
  mls_number = list(
    pattern = "^(ListingId|ListingKey|MLSNumber|MLS)$",
    reason = "Core identifier"
  ),

  # Core valuation factors
  property_condition = list(
    pattern = "^PropertyCondition$",
    reason = "Direct valuation factor"
  ),
  zoning = list(
    pattern = "^Zoning$",
    reason = "Critical for highest & best use"
  ),
  roof = list(
    pattern = "^Roof$",
    reason = "Direct property characteristic"
  ),
  interior_features = list(
    pattern = "^InteriorFeatures$",
    reason = "Valuation-relevant features"
  ),
  back_on_market = list(
    pattern = "^BackOnMarketDate$",
    reason = "Important market indicator"
  )
)

#' Classify a column based on pattern matching
#'
#' @param col_name Column name to classify
#' @param fill_rate Optional fill rate for additional logic
#' @param has_variance Boolean indicating if column has variance
#' @return Named list with: class (1, 2, or 3), reason, pattern_matched
classify_column_by_pattern <- function(col_name, fill_rate = NULL, has_variance = TRUE) {
  # Default: unclassified (NULL class means user should review)
  result <- list(
    class = NULL,
    reason = "No pattern match - requires manual classification",
    pattern_matched = NA,
    confidence = "low"
  )

  # Strip duplicate suffix (e.g., ".1", ".2") added by make.unique() for pattern matching
  # This ensures "LotSizeSquareFeet.1" matches the same patterns as "LotSizeSquareFeet"
  col_name_base <- sub("\\.[0-9]+$", "", col_name)

  # Create normalized version for pattern matching (remove spaces, special chars)
  # This allows "Close Date" to match patterns expecting "CloseDate"
  col_name_normalized <- gsub("[^[:alnum:]]", "", col_name_base)

  # Helper function to check if pattern matches (tries both original and normalized)
  matches_pattern <- function(pattern) {
    grepl(pattern, col_name_base, ignore.case = TRUE, perl = TRUE) ||
    grepl(pattern, col_name_normalized, ignore.case = TRUE, perl = TRUE)
  }

  # Check Class 4 patterns FIRST (PII/internal - never export)
  for (pattern_name in names(CLASS_4_PATTERNS)) {
    pattern_info <- CLASS_4_PATTERNS[[pattern_name]]
    tryCatch({
      if (matches_pattern(pattern_info$pattern)) {
        message(sprintf("CLASS 4 MATCH: '%s' matched pattern '%s' (%s)", col_name, pattern_name, pattern_info$pattern))
        return(list(
          class = 4,
          reason = pattern_info$reason,
          pattern_matched = pattern_name,
          confidence = "high"
        ))
      }
    }, error = function(e) {
      message(sprintf("REGEX ERROR in pattern '%s': %s", pattern_name, e$message))
    })
  }

  # Check Class 3 patterns (marginal - potentially useful)
  for (pattern_name in names(CLASS_3_PATTERNS)) {
    pattern_info <- CLASS_3_PATTERNS[[pattern_name]]
    tryCatch({
      if (matches_pattern(pattern_info$pattern)) {
        return(list(
          class = 3,
          reason = pattern_info$reason,
          pattern_matched = pattern_name,
          confidence = "high"
        ))
      }
    }, error = function(e) {
      message(sprintf("REGEX ERROR in CLASS_3 pattern '%s': %s", pattern_name, e$message))
    })
  }

  # Check Class 1 patterns (core analytical)
  for (pattern_name in names(CLASS_1_PATTERNS)) {
    pattern_info <- CLASS_1_PATTERNS[[pattern_name]]
    tryCatch({
      if (matches_pattern(pattern_info$pattern)) {
        return(list(
          class = 1,
          reason = pattern_info$reason,
          pattern_matched = pattern_name,
          confidence = "high"
        ))
      }
    }, error = function(e) {
      message(sprintf("REGEX ERROR in CLASS_1 pattern '%s': %s", pattern_name, e$message))
    })
  }

  # Check Class 2 patterns (supplemental)
  for (pattern_name in names(CLASS_2_PATTERNS)) {
    pattern_info <- CLASS_2_PATTERNS[[pattern_name]]
    tryCatch({
      if (matches_pattern(pattern_info$pattern)) {
        return(list(
          class = 2,
          reason = pattern_info$reason,
          pattern_matched = pattern_name,
          confidence = "high"
        ))
      }
    }, error = function(e) {
      message(sprintf("REGEX ERROR in CLASS_2 pattern '%s': %s", pattern_name, e$message))
    })
  }

  # Additional heuristics based on fill rate
  # Empty columns (0% fill) → Class 4 (exclude from export)
  # This keeps analysis clean; users can upgrade when data becomes available
  if (!is.null(fill_rate) && fill_rate == 0) {
    return(list(
      class = 4,
      reason = "Empty column (0% fill rate)",
      pattern_matched = "empty_column",
      confidence = "high"
    ))
  }

  # Very low fill (<5%) → Class 3 (marginal - may have occasional data)
  if (!is.null(fill_rate) && fill_rate < FILL_THRESHOLD_VERY_LOW) {
    result$class <- 3
    result$reason <- sprintf("Very low fill rate (%.1f%%)", fill_rate * 100)
    result$confidence <- "medium"
  }

  # Co-agent fields with very low fill → Class 3
  if (grepl("^Co(List|Buyer|Sell)", col_name_base) && !is.null(fill_rate) && fill_rate < 0.20) {
    return(list(
      class = 3,
      reason = "Co-agent field with low fill rate",
      pattern_matched = "co_agent_low_fill",
      confidence = "medium"
    ))
  }

  # FALLBACK: Check synonym-based classification from logic_target_groups
  # This catches columns that match mapping synonyms but not regex patterns
  col_normalized_for_lookup <- tolower(gsub("[^[:alnum:]]", "", col_name_base))
  if (!is.null(SYNONYM_CLASSIFICATION_LOOKUP[[col_normalized_for_lookup]])) {
    return(list(
      class = SYNONYM_CLASSIFICATION_LOOKUP[[col_normalized_for_lookup]],
      reason = "Matched via synonym lookup",
      pattern_matched = "synonym_lookup",
      confidence = "high"
    ))
  }

  return(result)
}

#' Classify all columns in a dataset
#'
#' @param dataset Data frame to classify
#' @param column_analysis Optional pre-computed column analysis (from analyze_all_columns)
#' @return Data frame with classification results
classify_all_columns <- function(dataset, column_analysis = NULL) {
  if (is.null(dataset) || ncol(dataset) == 0) {
    return(data.frame(
      column_name = character(0),
      class = integer(0),
      reason = character(0),
      pattern_matched = character(0),
      confidence = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Get or compute column analysis
  if (is.null(column_analysis)) {
    column_analysis <- analyze_all_columns(dataset)
  }

  # Classify each column
  results <- lapply(seq_len(nrow(column_analysis)), function(i) {
    row <- column_analysis[i, ]
    col_name <- row$column_name

    # Determine variance from analysis
    has_variance <- row$unique_count > 1 || nrow(dataset) <= MIN_ROWS_FOR_VARIANCE

    # Classify
    classification <- classify_column_by_pattern(
      col_name,
      fill_rate = row$fill_rate,
      has_variance = has_variance
    )

    data.frame(
      column_name = col_name,
      class = classification$class %||% NA_integer_,
      reason = classification$reason,
      pattern_matched = classification$pattern_matched %||% NA_character_,
      confidence = classification$confidence,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

#' Generate classification summary statistics
#'
#' @param classification_results Data frame from classify_all_columns
#' @return Named list with counts and percentages by class
summarize_classification <- function(classification_results) {
  if (is.null(classification_results) || nrow(classification_results) == 0) {
    return(list(
      total = 0,
      class_1 = 0, class_1_pct = 0,
      class_2 = 0, class_2_pct = 0,
      class_3 = 0, class_3_pct = 0,
      unclassified = 0, unclassified_pct = 0
    ))
  }

  total <- nrow(classification_results)
  class_1 <- sum(classification_results$class == 1, na.rm = TRUE)
  class_2 <- sum(classification_results$class == 2, na.rm = TRUE)
  class_3 <- sum(classification_results$class == 3, na.rm = TRUE)
  unclassified <- sum(is.na(classification_results$class))

  list(
    total = total,
    class_1 = class_1, class_1_pct = round(class_1 / total * 100, 1),
    class_2 = class_2, class_2_pct = round(class_2 / total * 100, 1),
    class_3 = class_3, class_3_pct = round(class_3 / total * 100, 1),
    unclassified = unclassified, unclassified_pct = round(unclassified / total * 100, 1)
  )
}

