##In this script, I want to tidy up the LTMP data and extract what I need.

library(tidyverse)
library (dplyr)
library (stringi)

##There are two df which I need to merge.

#One has a previous merge of COTS, trout, and HC. 
#The other has all the fish biomass data from the LTMP. Need them both to be the same.

#More fish biomass data but also has HC and SC.

fish <- latest.fish 
# 


benthic <- comp2021_Aug2026
name_key <- comp2021_groups


myfish <- fish %>%
  dplyr::mutate (SCIENTIFIC_NAME = paste(GENUS, SPECIES))%>%
  dplyr::filter (SCIENTIFIC_NAME == "Bolbometopon muricatum" |
                   SCIENTIFIC_NAME == "Scarus rivulatus" | 
                   SCIENTIFIC_NAME == "Siganus doliatus" | 
                   SCIENTIFIC_NAME == "Chaetodon trifascialis" | 
                   SCIENTIFIC_NAME == "Ctenochaetus striatus" | 
                   SCIENTIFIC_NAME == "Naso unicornis" | 
                   SCIENTIFIC_NAME == "Pomacanthus imperator" | 
                   SCIENTIFIC_NAME == "Pomacentrus moluccensis" |
                   SCIENTIFIC_NAME == "Lutjanus bohar" | 
                   SCIENTIFIC_NAME == "Plectorpomus leopardus" |
                   SCIENTIFIC_NAME == "Lethrinus miniatus" | 
                   SCIENTIFIC_NAME == "Caranx ignobilis" | 
                   SCIENTIFIC_NAME == "Lutjanus carponotatus" | 
                   SCIENTIFIC_NAME == "Plectorhinchus chaetodonoides" | 
                   SCIENTIFIC_NAME == "Epinephelus coioides" | 
                   SCIENTIFIC_NAME == "Sphyraena barracuda" | 
                   SCIENTIFIC_NAME == "Cheilinus undulatus" |
                   SCIENTIFIC_NAME == "Thalassoma hardwicke" | 
                   SCIENTIFIC_NAME == "Balistapus undulatus" | 
                   SCIENTIFIC_NAME == "Caesio cuning")

x <- unique(fish$AIMS_REEF_NAME) #Only 56 unique reefs 
x

# Mean abundance
fish_mean <- myfish %>% 
  group_by(A_SECTOR, AIMS_REEF_NAME, OPENORCLOSED, OPENORCLOSED_AFTER2004, SCIENTIFIC_NAME, cREPORT_YEAR, SITE_LAT, SITE_LONG) %>% 
  summarise(meanAbundance = mean(ABUNDANCE))

# Total fish abundance
fish_total <- myfish %>% 
  group_by(A_SECTOR, AIMS_REEF_NAME, OPENORCLOSED, OPENORCLOSED_AFTER2004, SCIENTIFIC_NAME, cREPORT_YEAR,  SITE_LAT, SITE_LONG) %>% 
  summarise(totalAbundance = sum(ABUNDANCE))

write.csv(fish_total, file = "Fish total.csv", row.names = TRUE)

reefnos <- unique(latest.fish$AIMS_REEF_NAME)
length((reefnos))


means_wide <- fish_mean %>%
  pivot_wider(
    names_from = SCIENTIFIC_NAME,
    values_from = meanAbundance)

sum_wide <- fish_total %>%
  pivot_wider(
    names_from = SCIENTIFIC_NAME,
    values_from = totalAbundance)

write.csv(sum_wide, file = "Fish sum wide.csv", row.names = TRUE)

#LTMP reefs linked with ReefModIDs
reefs <- read.csv("./Data/LTMP/ltmp_reefs.csv")

all <- left_join(means_wide, gbr_polygons)

all2 <- left_join(sum_wide, gbr_polygons)

final <- merge(all, all2, by = c("Reef", "Zone", "cYear", "Sector", "ReefModID"))

colnames(final)[3] <- "Year"

#Bring in ReefMod Reefs Metadata
GBR <- read.csv("./Data/GBR_REEFS_metadata.csv")

colnames(GBR)[1] <- "ReefModID"

final2 <- merge(final, GBR[,c(1:3)])

write.csv(final2, "./Data/LTMP/ltmp_fishbio_summarised.csv")

##Now extract the fish interested in, and total, then merge those into one df. 

##All this biomass is in  kg_1000m2

interest <- final2[,c(1, 3, 4, 6, 9:12, 15, 17, 20:23, 26, 28, 29)]

write.csv(interest, "./Data/LTMP/ltmp_fishbio_summarised_fisherytargets.csv")
