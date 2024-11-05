
# Calculate suitability raster 

can_wt <- 0.1654
canopy1_weighted <- canopy1 * can_wt

nat_ind_wt <- 0.4108
nat_ind1_weighted <- nat_ind1 * nat_ind_wt

dpa_wt <- 0.2109
dpa1_weighted <- dpa1 * dpa_wt

corridor_wt <- 0.1533
corridor1_weighted <- corridor1 * corridor_wt

slope_wt <- 0.0255
slope_weighted <- slope * slope_wt

river_wt <- 0.0341
river1_weighted <- river1 * river_wt


conservation_suitability <- canopy1_weighted + nat_ind1_weighted + dpa1_weighted + 
  corridor1_weighted + slope_weighted + river1_weighted
terra::plot(conservation_suitability, col = rev(terrain.colors(100)))

terra::writeRaster(conservation_suitability, "conservation_suitability_utm.tif")
