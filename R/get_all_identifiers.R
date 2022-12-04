# This script returns a dataframe of all identifiers found for a set of EUCTR trials

library(dplyr)
library(readr)
library(here)

source(here("R", "euctr_details.R"))
source(here("R", "euctr_reg_identifiers.R"))
source(here("R", "combine_identifiers.R"))

# Update input file name here (ids from EU Trials Tracker)
data <- read_csv(here("data", "2022-12-03_charite-euctr-trials.csv"))

trials <- data %>%
  pull(id)

found_ids <- get_ids_for_trials(trials)

# Update output file name here
write_csv(found_ids, here("data", "2022-12-03_charite-ids.csv"))