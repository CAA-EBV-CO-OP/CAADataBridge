# ============================================================================
# CValR Field Mapping
# Extracted from CAADataBridge server.R lines 8002-8045
# Maps ColumnMapper standard field names to CValR 4.0 expected column names
# ============================================================================

#' CValR rename mapping vector
#' Verified against CValR 4.0 DataImport.R ColNameMappings
CVALR_RENAME_MAP <- c(
  ListingId    = "MLS #",
  ListingKey   = "Listing Key Numeric",
  PriceSold    = "Sold Price",
  PriceOriginal = "Original List Price",
  PriceListed  = "List Price",
  DateSold     = "Sold Date",
  DateListed   = "On Market Date",
  DatePending  = "Purchase Contract Date",
  OffMarketDate = "Off Market Date",
  PostalCode   = "Postal Code",
  SubArea      = "Minor / Sub Area",
  Region       = "Major Area / Region",
  YrBlt        = "Year Built",
  StyleStoreys = "Architectural Style",
  Beds         = "Bedrooms Total",
  BA           = "Bathrooms Full",
  PB           = "Bathrooms Half",
  EnsBa        = "Ensuite Bathrooms",
  SqFtTotal    = "Total Finished SqFt",
  AboveGradeFinishedArea = "Above Grade Finished Sqft",
  BelowGradeFinishedArea = "Below Grade Finished SqFt",
  BasementYN   = "Basement YN",
  BasementFeatures = "Basement",
  GarSp        = "Garage Spaces",
  GarageYN     = "Garage YN",
  PoolYN       = "Pool YN",
  LotS         = "Lot Acres",
  WaterfrontYN = "Waterfront YN",
  WaterFrontage = "Waterfrontage",
  WaterInfluence = "Waterfront Features",
  SewerType    = "Sewer",
  WaterSupply  = "Water Source",
  PID          = "Parcel ID",
  LegalDescription = "Tax Legal Description",
  RemarksPublic = "Public Remarks",
  RemarksSales = "REALTOR\u00ae Remarks",
  SpecialListingConditions = "Special Listing Conditions",
  DwellingType = "Property Sub Type",
  Construction = "Construction Materials",
  StrataFees   = "Strata Fees",
  TotalKitchens = "Total Kitchens",
  LotDimensions = "Lot Size Dimensions L x W",
  TitleChangeDate = "Title/Ownership Change Date"
)

#' Apply CValR rename mapping to a data frame
#'
#' @param data Data frame with ColumnMapper standard field names
#' @return List with data (renamed), renamed_count
apply_cvalr_rename <- function(data) {
  col_names <- names(data)
  renamed_count <- 0
  for (i in seq_along(col_names)) {
    new_name <- CVALR_RENAME_MAP[col_names[i]]
    if (!is.na(new_name)) {
      col_names[i] <- new_name
      renamed_count <- renamed_count + 1
    }
  }
  names(data) <- col_names

  # Safety net: CValR requires "MLS #"
  if ("Listing Key Numeric" %in% names(data) && !("MLS #" %in% names(data))) {
    data[["MLS #"]] <- data[["Listing Key Numeric"]]
    message("CValR safety net: Duplicated 'Listing Key Numeric' into 'MLS #'")
  }

  list(data = data, renamed_count = renamed_count)
}
