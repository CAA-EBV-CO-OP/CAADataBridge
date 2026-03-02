
library(tidyverse)
library(qeML)
library(partykit)

#' Analyze CMS Stability via Subsampling
#' 
#' This function mitigates the "single tree instability" flaw by running 
#' multiple iterations of the decision tree algorithm on slightly perturbed data.
#'
#' @param adf Data frame: The full Assignment Data Frame
#' @param subject_data Data frame: Single row containing subject property data
#' @param response_var Character: Name of the response variable
#' @param predictors Character vector: Names of predictor variables
#' @param iterations Integer: Number of subsampling iterations (default 20)
#' @param subsample_ratio Numeric: Fraction of data to keep in each iteration (default 0.9)
#' @param min_split Integer: Minimum split parameter for qeDT/ctree (default 20)
#' 
#' @return List containing:
#'   - stability_scores: DF with "Core" status for each record
#'   - summary_stats: Avg CMS size, etc.
analyze_cms_stability <- function(adf, subject_data, response_var, predictors, 
                                iterations = 20, subsample_ratio = 0.90, min_split = 20) {
  
  # Ensure robust inputs
  if (nrow(adf) < 50) {
    return(list(error = "Sample size too small for stability analysis (< 50 records)."))
  }
  
  # Add a unique ID to track records across iterations if not present
  if (!"_ROW_ID_" %in% names(adf)) {
    adf$`_ROW_ID_` <- 1:nrow(adf)
  }
  
  # Initialize counters
  cms_appearances <- rep(0, nrow(adf))
  total_runs_record_included <- rep(0, nrow(adf)) # Denominator (how many times was this record in the training set?)
  
  # Prepare data for qeML (must be clean)
  # qeDT handles factors, but we need to ensure consistent levels
  
  # Loop
  valid_iterations <- 0
  
  withProgress(message = 'Running Stability Analysis', value = 0, {
    
    for (i in 1:iterations) {
      incProgress(1/iterations, detail = paste("Iteration", i))
      
      # 1. Subsample
      n_sample <- floor(nrow(adf) * subsample_ratio)
      sample_indices <- sample(1:nrow(adf), n_sample)
      train_data <- adf[sample_indices, ]
      
      # Update denominator: These records had a *chance* to be in CMS
      total_runs_record_included[sample_indices] <- total_runs_record_included[sample_indices] + 1
      
      # 2. Build Tree
      # We need to use tryCatch as small samples might fail to split
      dt_model <- tryCatch({
        # We need to subset columns to only predictors + response for qeDT
        # But we need to keep _ROW_ID_ for tracking (handle externally)
        train_for_model <- train_data[, c(response_var, predictors)]
        
        # Ensure column names are sanitized
        names(train_for_model) <- make.names(names(train_for_model))
        resp_sanitized <- make.names(response_var)
        
        qeDT(train_for_model, resp_sanitized, holdout = NULL, minBucket = min(7, nrow(train_for_model)/10))
      }, error = function(e) { NULL })
      
      if (is.null(dt_model)) next
      
      # 3. Predict Subject Node
      # Align subject columns with training data
      # (This logic mimics the main app's subject alignment)
      subj_aligned <- subject_data
      
      # Ensure subject has same columns
      missing_cols <- setdiff(names(train_for_model), names(subj_aligned))
      # This is tricky without the full alignment logic from app.R, 
      # but assuming subject_data is already well-formed from the app:
      
      # We must predict using the party object
      ct_model <- dt_model$ctout
      
      # Align subject factors levels to model
      # Note: This is simplified. In prod, we reuse the robust alignment function.
      tryCatch({
        subj_node <- predict(ct_model, newdata = subj_aligned, type = "node")
        
        # 4. Identify CMS
        # Get nodes for training data
        train_nodes <- predict(ct_model, newdata = train_for_model, type = "node")
        
        # Which training records are in the same node?
        cms_indices_in_sample <- which(train_nodes == subj_node)
        
        # Map back to original Row IDs
        original_indices <- train_data$`_ROW_ID_`[cms_indices_in_sample]
        
        # Update numerator
        cms_appearances[original_indices] <- cms_appearances[original_indices] + 1
        
        valid_iterations <- valid_iterations + 1
        
      }, error = function(e) { 
        # Prediction failed (usually factor level mismatch)
        NULL 
      })
    }
  })
  
  if (valid_iterations == 0) {
    return(list(error = "All stability iterations failed. Check data quality."))
  }
  
  # Calculate Scores
  # Stability Score = (Times in CMS) / (Times in Training Set)
  # Note: If a record was never sampled (unlikely), score is 0
  stability_scores <- ifelse(total_runs_record_included > 0,
                             cms_appearances / total_runs_record_included,
                             0)
  
  results_df <- data.frame(
    RowID = 1:nrow(adf),
    StabilityScore = stability_scores,
    Frequency = cms_appearances,
    TotalRuns = total_runs_record_included
  )
  
  # Join back to original data for display (subset of columns)
  results_enriched <- cbind(adf, results_df)
  
  return(list(
    results = results_enriched,
    iterations = valid_iterations,
    max_score = max(stability_scores)
  ))
}
