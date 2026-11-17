library(tidyverse)
library(sf)
library(here)

nearest_reef <- function(lat, lon, reefs = readRDS(here("Reef Finder Function","GBR_ReefMod.rds")), proj_crs = 3577) {
  stopifnot(length(lat) == length(lon))
  
  reefs <- sf::st_as_sf(reefs)   # data.frame with sfc column -> sf object
  
  out <- tibble::tibble(lat = lat, lon = lon,
                        ReefMod_ID = NA, ReefID = NA,
                        GBR_NAME = NA_character_, dist_m = NA_real_)
  
  ok <- !is.na(lat) & !is.na(lon)   # skip missing coordinates
  if (!any(ok)) return(out)
  
  pts <- st_as_sf(data.frame(lon = lon[ok], lat = lat[ok]),
                  coords = c("lon", "lat"), crs = 4326) |>
    st_transform(proj_crs)
  reefs_p <- st_transform(reefs, proj_crs)
  
  idx <- st_nearest_feature(pts, reefs_p)
  d   <- st_distance(pts, reefs_p[idx, ], by_element = TRUE)
  
  out$ReefMod_ID[ok] <- reefs_p$ReefMod_ID[idx]
  out$ReefID[ok]     <- reefs_p$ReefID[idx]
  out$GBR_NAME[ok]   <- as.character(reefs_p$GBR_NAME[idx])
  out$dist_m[ok]     <- as.numeric(d)   # 0 if inside the polygon
  out
}

coords = unique(habitat[,7:8])
reefs_approx <- nearest_reef(lat = coords$REEF_LAT, lon = coords$REEF_LONG)
