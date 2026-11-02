# This is for tidying up the coral cover data and aggregating per species/type.

library(dplyr)
library(tidyr)


habitat <- comp2021_Aug2026
habnames <- comp2021_groups %>%
  glimpse()

gbr_polygons <- read.csv("C:/Users/s4657929/OneDrive - The University of Queensland/Nidhi-LTMP-analysis/LTMP cleaning scripts/GBR_REEF_POLYGONS.csv")

unique(gbr_polygons$GBR_NAME)
unique(habitat$REEF_NAME)

reefmod_reefs <- habitat %>%
  dplyr::mutate()
  