# Preliminary fish maps across GBR extent

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rnaturalearth)
library(ggspatial)
library(gganimate)
library(transformr)
library(gifski)

fish_total <- fish_total %>%
  dplyr::filter(!is.na(SITE_LAT))

sum_wide <- sum_wide %>%
  dplyr::filter(!is.na(SITE_LAT))

fish_sf <- fish_total %>%
  st_as_sf(coords = c("SITE_LONG", "SITE_LAT"), crs = 4326)

unique(sum_wide$A_SECTOR)
reef_year <- fish_sf$cREPORT_YEAR

land <- ne_countries(scale = "medium", returnclass = "sf")
bb   <- st_bbox(fish_sf)

one_sp <- filter(fish_sf, SCIENTIFIC_NAME == "Bolbometopon muricatum") 
one_sp$cREPORT_YEAR <- as.numeric(one_sp$cREPORT_YEAR)

p <- ggplot() +
  geom_sf(data = land, fill = "grey90", colour = "grey60") +
  geom_sf(data = one_sp, aes(size = totalAbundance, colour = totalAbundance), alpha = 0.8) +
  scale_colour_viridis_c() +
  coord_sf(xlim = c(bb[["xmin"]] - 0.5, bb[["xmax"]] + 0.5),
           ylim = c(bb[["ymin"]] - 0.5, bb[["ymax"]] + 0.5), expand = FALSE) +
  labs(title = "Year: {frame_time}") +
  theme_minimal() +
  transition_time(cREPORT_YEAR)

p

anim <- animate(p, nframes = 100, fps = 5, width = 700, height = 800, renderer = gifski_renderer())
class(anim)
anim

anim_save("bolbo.gif", animation = anim,
          path = "C:/Users/s4657929/Documents")
