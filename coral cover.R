# This is for tidying up the coral cover data and aggregating per species/type.

library(dplyr)
library(tidyr)
library(stringi)
library(stringr)

# Part 1: extracting and saving all reef names to be cleaned
   
habitat1 <- comp2021_Aug2026 %>%
  dplyr::mutate(AIMS_REEF_NAME = toupper(AIMS_REEF_NAME))
  
habnames <- comp2021_groups %>%
  glimpse()

# ReefMod names
gbr_polygons <- read.csv("C:/Users/s4657929/OneDrive - The University of Queensland/Nidhi-LTMP-analysis/Data/GBR_REEF_POLYGONS.csv")

reefmod_names <- unique(gbr_polygons$GBR_NAME)
unique(habitat1$REEF_NAME)
  
aims_reef_name <- unique(toupper(habitat1$AIMS_REEF_NAME))
print(aims_reef_name)

reef_name <- unique(habitat1$REEF_NAME)

reeflist <- list(first = aims_reef_name, second = reef_name, third = reefmod_names)
n <- max(lengths(reeflist))

reefnames <- as.data.frame(lapply(reeflist, `length<-`, n))
colnames(reefnames) <- c("aims_reef_name", "reef_name", "reefmod_reef_name")

write.csv(reefnames, file = "All reef names.csv", row.names = TRUE)

# bringing back cleaned reef names 
cleaned_reefnames <- read.csv("C:/Users/s4657929/OneDrive - The University of Queensland/Nidhi-LTMP-analysis/Reef names/Cleaned reef names.csv")
colnames(cleaned_reefnames)[1] <- "AIMS_REEF_NAME"

head(habitat)

unique(habitat1$SHELF) # I, M, O

p_code <- c(unique(habitat1$P_CODE)) # IN, RAP, RM, RMRAP

# Average coral cover at each shelf position for each reef per year
habitat <- left_join (habitat1, cleaned_reefnames, by = "AIMS_REEF_NAME") %>%
  dplyr::filter(!(AIMS_REEF_NAME == "MIDDLE REEF")) %>%
  group_by(SHELF, AIMS_REEF_NAME, FULLREEF_ID, REEF_LAT, REEF_LONG, REPORT_YEAR, SITE_DEPTH, COMP_2021, reefmod_ID)%>%
  summarise(avgcover = mean(COVER))

head(habitat)

write.csv(habitat, file = "Habitat data", row.names = TRUE)

# checking which reefs fall under which P_CODE
LTMP_split <- habitat1 %>%
  distinct(P_CODE, AIMS_REEF_NAME) %>%
  group_by(P_CODE) %>%
  summarise(reefs = list(AIMS_REEF_NAME)) 

# overlapping reefs
overlapping <- habitat1 %>%
  distinct(P_CODE, AIMS_REEF_NAME) %>%
  group_by(AIMS_REEF_NAME) %>%
  filter(n() > 1) %>%
  summarise(codes = paste(sort(P_CODE), collapse = ", "), n_codes = n()) %>%
  arrange(desc(n_codes))

# finding overlapping years 
reef_codes <- habitat1 %>%
  distinct(AIMS_REEF_NAME, P_CODE, REPORT_YEAR) %>%
  group_by(AIMS_REEF_NAME, REPORT_YEAR) %>%
  mutate(multi_in_year = n_distinct(P_CODE) > 1) %>%      
  group_by(AIMS_REEF_NAME) %>%
  filter(n_distinct(P_CODE) > 1) %>%        
  group_by(AIMS_REEF_NAME, P_CODE) %>%
  summarise(shared_years = paste(sort(unique(REPORT_YEAR[multi_in_year])), collapse = ", "),
            .groups = "drop")%>%
  dplyr::filter(shared_years != "")




























