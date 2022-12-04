# Functions for the retrieval of trial identifiers for EUCTR trials

# Gets identifiers for n EUCTR trials
get_ids_for_trials <- function(trials) {
  
  found_ids <- data.frame(
    euctr_id=character(),
    other_id=character(),
    field=character(),
    provenance=character()
  )
  
  for (trial in trials) {
    
    res <- get_ids(trial)
    
    found_ids <- rbind(found_ids, res)
    
    # Adjust the sleep as needed
    Sys.sleep(4 + runif(1, 0, 5))
  }
  
  return(found_ids)
}


# Combine identifier search results from different pages for a single EUCTR trial
get_ids <- function(trial) {
  
  # Input is EUCTR trial identifier
  # First column: EUCTR trial identifier
  # Second column: identifier found
  # Third column: response field the identifier was found in (US NCT number | ISRCTN number | other)
  # Fourth column provenance of the identifier found (download | results_page)
  # Output: dataframe with 4 rows per EUCTR trial
  
  found_ids <- data.frame(
    euctr_id=character(),
    other_id=character(),
    field=character(),
    provenance=character()
  )

  # Get identifiers from Full Trial Details download
  ids <- euctr_reg_identifiers(trial)
  
  found_ids <- found_ids %>%
    add_row(euctr_id = trial,
            other_id = ids[1],
            field="US NCT number",
            provenance = "download") %>%
    add_row(euctr_id = trial,
            other_id = ids[2],
            field = "other",
            provenance = "downlaod")
  
  # Get identifiers from the Results page (if available)
  rp_ids <- euctr_details(trial)
  
  rp_id_ctg <- rp_ids$ctg_identifier
  rp_id_isrctn <- rp_ids$isrctn_identifier
  
  found_ids <- found_ids %>%
    add_row(euctr_id = trial,
            other_id = rp_id_ctg,
            field = "US NCT number",
            provenance = "results_page") %>%
    add_row(euctr_id = trial,
            other_id = rp_id_isrctn,
            field = "ISRCTN number",
            provenance = "results_page")
  
  
  return(found_ids)
}