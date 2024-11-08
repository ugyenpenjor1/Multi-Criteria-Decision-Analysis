

# Function to compute eigenvector for AHP using iteration

################################################################################

# Trial, this uses the same input matrix for subsequent iterations

# compute_eigenvector <- function(mat, tol = 1e-4, max_iter = 1000) {
#   # Step 1: Initialize
#   eigenvector <- rep(1 / nrow(mat), nrow(mat)) # Start with uniform eigenvector
#   iter <- 0
#   diff <- tol + 1 # Start with a difference greater than tolerance
#   
#   # Step 2: Iterate until convergence
#   while (diff > tol && iter < max_iter) {
#     # Square the matrix
#     mat_squared <- mat %*% mat
#     
#     # Step 3: Row sums
#     row_sums <- rowSums(mat_squared)
#     
#     # Step 4: Total sum of the row sums
#     total_sum <- sum(row_sums)
#     
#     # Step 5: Normalize to get the new eigenvector
#     new_eigenvector <- row_sums / total_sum
#     
#     # Step 6: Compute the difference between the new and old eigenvectors
#     diff <- max(abs(new_eigenvector - eigenvector))
#     
#     # Update eigenvector
#     eigenvector <- new_eigenvector
#     iter <- iter + 1
#   }
#   
#   if (iter == max_iter) {
#     warning("Maximum iterations reached without convergence.")
#   }
#   
#   return(eigenvector)
# }
# 
# # Sample matrix from your input
# mat <- matrix(c(1.0000, 2.0000, 0.5000, 2.0000,
#                 0.5000, 1.0000, 0.5000, 0.3333,
#                 2.0000, 2.0000, 1.0000, 2.0000,
#                 0.5000, 3.0000, 0.5000, 1.0000), 
#               nrow = 4, byrow = TRUE)
# 
# # Compute eigenvector
# eigenvector <- compute_eigenvector(mat)
# eigenvector


################################################################################
################################################################################
################################################################################

# With a plot

library(ggplot2)
library(reshape2)

compute_eigenvector <- function(mat, tol = 1e-6, max_iter = 1000) {
  # Step 1: Initialise
  eigenvector <- rep(1 / nrow(mat), nrow(mat)) # Start with a uniform eigenvector
  iter <- 0
  diff <- tol + 1 # Start with a difference greater than tolerance
  all_eigenvectors <- list() # To store eigenvectors at each iteration
  
  # Step 2: Iterate until convergence
  while (diff > tol && iter < max_iter) {
    # Square the matrix (use the matrix from previous iteration or the input matrix in the first iteration)
    mat <- mat %*% mat
    
    # Step 3: Row sums
    row_sums <- rowSums(mat)
    
    # Step 4: Total sum of the row sums
    total_sum <- sum(row_sums)
    
    # Step 5: Normalise to get the new eigenvector
    new_eigenvector <- row_sums / total_sum
    
    # Store the current eigenvector for plotting
    all_eigenvectors[[iter + 1]] <- new_eigenvector
    
    # Step 6: Compute the difference between the new and old eigenvectors
    diff <- max(abs(new_eigenvector - eigenvector))
    
    # Update eigenvector
    eigenvector <- new_eigenvector
    iter <- iter + 1
    
    # Print iteration details for debugging
    cat("Iteration:", iter, "\n")
    cat("Eigenvector:\n", new_eigenvector, "\n")
    cat("Difference:", diff, "\n\n")
  }
  
  if (iter == max_iter) {
    warning("Maximum iterations reached without convergence.")
  }
  
  # Convert list of eigenvectors to a data frame for plotting
  eigen_df <- do.call(rbind, all_eigenvectors)
  eigen_df <- as.data.frame(eigen_df)
  eigen_df$Iteration <- seq_len(nrow(eigen_df))
  
  return(eigen_df)
}


################################################################################

# Apply the function

# Sample matrix
# Replace with your original matrix
mat <- matrix(c(1.0000, 2.0000, 0.5000, 2.0000,
                0.5000, 1.0000, 0.5000, 0.3333,
                2.0000, 2.0000, 1.0000, 2.0000,
                0.5000, 3.0000, 0.5000, 1.0000), 
              nrow = 4, byrow = TRUE)

# Assign dimension names
dimnames(mat)

dimnames(mat) <- list(
  c("Canopy", "Nat_ind", "DPA", "Corridor"),
  c("Canopy", "Nat_ind", "DPA", "Corridor")
)

# Compute eigenvector and plot eigenvalues over iterations
eigenvector <- compute_eigenvector(mat, tol = 1e-6)
eigenvector

# Melt data for ggplot
eigen_df_long <- reshape2::melt(
  eigenvector, 
  id.vars = "Iteration", 
  variable.name = "Element", 
  value.name = "Eigenvalue"
)

# Plot eigenvalues over iterations
ggplot(eigen_df_long, aes(x = Iteration, y = Eigenvalue, color = Element)) +
  geom_line(linewidth = 1) +
  labs(title = "Eigenvalues over Iterations", x = "Iteration", y = "Eigenvalue") +
  theme_minimal()

################################################################################
################################################################################
################################################################################

# Comino et al., 2014, Land Use Policy (page 389) values tested to my function

mat <- matrix(c(1, 7, 7, 3,
                1/7, 1, 1/3, 1/5,
                1/7, 3, 1, 1/5,
                1/3, 5, 5, 1), 
              nrow = 4, byrow = TRUE)

# Assign dimension names
dimnames(mat)

dimnames(mat) <- list(
  c("Urbanised_areas", "Hydroecological_risk", "Land_use", "Road_system"),
  c("Urbanised_areas", "Hydroecological_risk", "Land_use", "Road_system")
)

################################################################################
################################################################################
################################################################################

# My data

# Conservation
mat <- matrix(c(1, 1/5, 3, 1/3, 5, 5, 
                5, 1, 3, 5, 7, 7,
                1/3, 1/3, 1, 5, 7, 7,
                3, 1/5, 1/5, 1, 5, 7,
                1/5, 1/7, 1/7, 1/5, 1, 1/3,
                1/5, 1/7, 1/7, 1/7, 3, 1),
              nrow = 6, byrow = TRUE)

mat <- matrix(c(1, 1/3, 3, 2, 7, 5, 
                3, 1, 4, 5, 7, 6,
                1/3, 1/4, 1, 1/2, 7, 7,
                1/2, 1/5, 2, 1, 7, 7,
                1/7, 1/7, 1/7, 1/7, 1, 1/3,
                1/5, 1/6, 1/7, 1/7, 3, 1),
              nrow = 6, byrow = TRUE)

# Assign dimension names
dimnames(mat)

dimnames(mat) <- list(
  c("Canopy", "Nat_Ind", "DPA", "Corridor", "Slope", "River"),
  c("Canopy", "Nat_Ind", "DPA", "Corridor", "Slope", "River")
)


# Compute eigenvector and plot eigenvalues over iterations
eigenvector <- compute_eigenvector(mat, tol = 1e-6)
eigenvector

# Melt data for ggplot
eigen_df_long <- reshape2::melt(
  eigenvector, 
  id.vars = "Iteration", 
  variable.name = "Element", 
  value.name = "Eigenvalue"
)

# Plot eigenvalues over iterations
ggplot(eigen_df_long, aes(x = Iteration, y = Eigenvalue, color = Element)) +
  geom_line(linewidth = 1) +
  labs(title = "Eigenvalues over Iterations", x = "Iteration", y = "Eigenvalue") +
  theme_minimal()


################################################################################

# Pressure
# mat <- matrix(c(1, 5, 3, 1/5, 3, 5, 
#                 1/5, 1, 1/5, 1/5, 3, 3,
#                 1/3, 5, 1, 1/5, 3, 5,
#                 5, 5, 5, 1, 5, 7,
#                 1/3, 1/3, 1/3, 1/5, 1, 7,
#                 1/5, 1/3, 1/5, 1/7, 1/7, 1),
#               nrow = 6, byrow = TRUE)

# Pressure Index

mat <- matrix(c(1, 5, 3, 3, 5, 
                1/5, 1, 1/5, 3, 3,
                1/3, 5, 1, 3, 5,
                1/3, 1/3, 1/3, 1, 7,
                1/5, 1/3, 1/5, 1/7, 1),
              nrow = 5, byrow = TRUE)

mat <- matrix(c(1, 1/2, 3, 3, 3, 
                2, 1, 1/2, 1/2, 3,
                1/3, 2, 1, 3, 5,
                1/3, 2, 1/3, 1, 3,
                1/3, 1/3, 1/5, 1/3, 1),
              nrow = 5, byrow = TRUE)

# Assign dimension names
dimnames(mat)

dimnames(mat) <- list(
  c("Urban", "Fire", "Pop-Den", "Pop_Size", "DRoad"),
  c("Urban", "Fire", "Pop-Den", "Pop_Size", "DRoad")
)
mat

# Compute eigenvector and plot eigenvalues over iterations
eigenvector <- compute_eigenvector(mat, tol = 1e-6)
eigenvector

# Melt data for ggplot
eigen_df_long <- reshape2::melt(
  eigenvector, 
  id.vars = "Iteration", 
  variable.name = "Element", 
  value.name = "Eigenvalue"
)

# Plot eigenvalues over iterations
ggplot(eigen_df_long, aes(x = Iteration, y = Eigenvalue, color = Element)) +
  geom_line(linewidth = 1) +
  labs(title = "Eigenvalues over Iterations", x = "Iteration", y = "Eigenvalue") +
  theme_minimal()

# Plot weights
# Extract data for Iteration 2
data_iteration_2 <- eigenvector[5, ]

# Convert the data into long format for ggplot
data_long <- data.frame(
  Category = c("Urban", "Fire", "Pop-Den", "Pop_Size", "DRoad"),
  Value = as.numeric(data_iteration_2[1:5])
)

# Ensure the x-axis categories appear in the original order
data_long$Category <- factor(data_long$Category, levels = c("Urban", "Fire", "Pop-Den", "Pop_Size", "DRoad"))

# Plot the barplot using ggplot2
ggplot(data_long, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = round(Value, 4)), vjust = -0.5, size = 5) + # Add labels above bars
  labs(title = "Weights of Pressure Criteria", x = "Criteria", y = "Weights") +
  scale_fill_manual(
    values = c(
      "Urban" = "#cc3300", 
      "Fire" = "#cc0066", 
      "Pop-Den" = "#996600", 
      "Pop_Size" = "#cc6600", 
      "DRoad" = "#ffcc33"
    )
  ) + # Set custom colours (matching Miro)
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.75),
    axis.text = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16),
    legend.text = ggplot2::element_text(size = 16),
    legend.title = element_text(size = 16),
    plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm")
  )

ggplot2::ggsave(
  "Weights_Pressure.png",
  plot = ggplot2::last_plot(),
  width = 6,
  height = 7,
  dpi = 300
)

################################################################################


# Reverse the factor levels for correct order in the flipped plot
data_long$Category <- factor(data_long$Category, levels = rev(c("Urban", "Fire", "Pop-Den", "Pop_Size", "DRoad")))

# Plot the barplot with flipped axes using ggplot2
ggplot(data_long, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  #geom_text(aes(label = round(Value, 4)), hjust = -0.5) + # Add labels beside bars after flipping
  labs(title = "Weights of Pressure Criteria", x = "Criteria", y = "Weights") +
  scale_fill_manual(
    values = c(
      "Urban" = "#cc3300", 
      "Fire" = "#cc0066", 
      "Pop-Den" = "#996600", 
      "Pop_Size" = "#cc6600", 
      "DRoad" = "#ffcc33"
    )
  ) + 
  coord_flip() + # Flip the axes
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.75),
    axis.text = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16),
    legend.text = ggplot2::element_text(size = 16),
    legend.title = element_text(size = 16),
    plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm")
  )

ggplot2::ggsave(
  "Weights_Pressure_181024.png",
  plot = ggplot2::last_plot(),
  width = 6,
  height = 7,
  dpi = 300
)




################################################################################
################################################################################
################################################################################

# Conservation
mat <- matrix(c(1, 1/5, 3, 1/3, 5, 5, 
                5, 1, 3, 5, 7, 7,
                1/3, 1/3, 1, 5, 7, 7,
                3, 1/5, 1/5, 1, 5, 7,
                1/5, 1/7, 1/7, 1/5, 1, 1/3,
                1/5, 1/7, 1/7, 1/7, 3, 1),
              nrow = 6, byrow = TRUE)

mat <- matrix(c(1, 1/3, 3, 2, 7, 5, 
                3, 1, 4, 5, 7, 6,
                1/3, 1/4, 1, 1/2, 7, 7,
                1/2, 1/5, 2, 1, 7, 7,
                1/7, 1/7, 1/7, 1/7, 1, 1/3,
                1/5, 1/6, 1/7, 1/7, 3, 1),
              nrow = 6, byrow = TRUE)


# Assign dimension names
dimnames(mat)

dimnames(mat) <- list(
  c("Canopy", "Nat_Ind", "DPA", "Corridor", "Slope", "River"),
  c("Canopy", "Nat_Ind", "DPA", "Corridor", "Slope", "River")
)
mat

# Compute eigenvector and plot eigenvalues over iterations
eigenvector <- compute_eigenvector(mat, tol = 1e-6)
eigenvector

# Melt data for ggplot
eigen_df_long <- reshape2::melt(
  eigenvector, 
  id.vars = "Iteration", 
  variable.name = "Element", 
  value.name = "Eigenvalue"
)

# Plot eigenvalues over iterations
ggplot(eigen_df_long, aes(x = Iteration, y = Eigenvalue, color = Element)) +
  geom_line(linewidth = 1) +
  labs(title = "Eigenvalues over Iterations", x = "Iteration", y = "Eigenvalue") +
  theme_minimal()

# Plot weights
# Extract data for Iteration 2
data_iteration_2 <- eigenvector[2, ]

# Convert the data into long format for ggplot
data_long <- data.frame(
  Category = c("Canopy", "Nat_Ind", "DPA", "Corridor", "Slope", "River"),
  Value = as.numeric(data_iteration_2[1:6])
)

# Ensure the x-axis categories appear in the original order
data_long$Category <- factor(
  data_long$Category, 
  levels = c("Canopy", "Nat_Ind", "DPA", "Corridor", "Slope", "River")
)

# Plot the barplot using ggplot2
ggplot(data_long, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = round(Value, 4)), vjust = -0.5, size = 5) + # Add labels above bars
  labs(title = "Weights of Conservation Criteria", x = "Criteria", y = "Weights") +
  scale_fill_manual(
    values = c(
      "Canopy" = "#ccff00", 
      "Nat_Ind" = "#66cc33", 
      "DPA" = "#009977", 
      "Corridor" = "#00cccc", 
      "Slope" = "#999999",
      "River" = "#0066ff"
    )
  ) + # Set custom colours (matching Miro)
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.75),
    axis.text = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16),
    legend.text = ggplot2::element_text(size = 16),
    legend.title = element_text(size = 16),
    plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm")
  )

ggplot2::ggsave(
  "Weights_Pressure.png",
  plot = ggplot2::last_plot(),
  width = 6,
  height = 7,
  dpi = 300
)

################################################################################


# Reverse the factor levels for correct order in the flipped plot
data_long$Category <- factor(
  data_long$Category, 
  levels = rev(c("Canopy", "Nat_Ind", "DPA", "Corridor", "Slope", "River"))
)

# Plot the barplot with flipped axes using ggplot2
ggplot(data_long, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  #geom_text(aes(label = round(Value, 4)), hjust = -0.5) + # Add labels beside bars after flipping
  labs(title = "Weights of Conservation Criteria", x = "Criteria", y = "Weights") +
  scale_fill_manual(
    values = c(
      "Canopy" = "#ccff00", 
      "Nat_Ind" = "#66cc33", 
      "DPA" = "#009977", 
      "Corridor" = "#00cccc", 
      "Slope" = "#999999",
      "River" = "#0066ff"
    )
  ) + 
  coord_flip() + # Flip the axes
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.75),
    axis.text = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16),
    legend.text = ggplot2::element_text(size = 16),
    legend.title = element_text(size = 16),
    plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm")
  )

ggplot2::ggsave(
  "Weights_Conservation_161024.png",
  plot = ggplot2::last_plot(),
  width = 6,
  height = 7,
  dpi = 300
)


################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################

# CONSERVATION

# Load raster and do some edits

canopy <- terra::rast("canopy_height_utm_01.tif")
nat_ind <- terra::rast("naturalness_index_utm_01.tif")
dpa <- terra::rast("distance_to_pa_utm_01.tif")
corridor <- terra::rast("corridor_utm_01.tif")
slope <- terra::rast("slope_utm_01.tif")
river <- terra::rast("river_density_utm_01.tif")

# Inspect extents
terra::ext(canopy)
terra::ext(nat_ind)
terra::ext(dpa)
terra::ext(corridor)
terra::ext(slope)
terra::ext(river)

# Investigate CRS
terra::crs(canopy)
terra::crs(nat_ind)
terra::crs(dpa)
terra::crs(corridor)
terra::crs(slope)
terra::crs(river)

# Extents don't match
# Resample to match extents
canopy1 <- terra::resample(
  x = canopy,
  y = slope
)

nat_ind1 <- terra::resample(
  x = nat_ind,
  y = slope
)

dpa1 <- terra::resample(
  x = dpa,
  y = slope
)

corridor1 <- terra::resample(
  x = corridor,
  y = slope
)

river1 <- terra::resample(
  x = river,
  y = slope
)

# Inspect extents after resampling
terra::ext(canopy1)
terra::ext(nat_ind1)
terra::ext(dpa1)
terra::ext(corridor1)
terra::ext(slope)
terra::ext(river1)

# Ensure they can be stacked (only possible when all extents match)
all <- c(canopy1, nat_ind1, dpa1, corridor1, slope, river1)
terra::plot(all)

# Rasters don't have proper names, so assign appropriate names
names(canopy1) <- "canopy"
names(nat_ind1) <- "nat_ind"
names(dpa1) <- "DPA"
names(corridor1) <- "corridor"
names(river1) <- "river_den"

# Now stacked again and check names
all <- c(canopy1, nat_ind1, dpa1, corridor1, slope, river1)
terra::plot(all) # seems all good

# check resolution
terra::res(all)

################################################################################
################################################################################
################################################################################


# Calculate suitability raster 

# Weight raster by eigenvector

can_wt <- eigenvector$Canopy[5]
canopy1_weighted <- canopy1 * can_wt

nat_ind_wt <- eigenvector$Nat_Ind[5]
nat_ind1_weighted <- nat_ind1 * nat_ind_wt

dpa_wt <- eigenvector$DPA[5]
dpa1_weighted <- dpa1 * dpa_wt

corridor_wt <- eigenvector$Corridor[5]
corridor1_weighted <- corridor1 * corridor_wt

slope_wt <- eigenvector$Slope[5]
slope_weighted <- slope * slope_wt

river_wt <- eigenvector$River[5]
river1_weighted <- river1 * river_wt


conservation_suitability <- canopy1_weighted + nat_ind1_weighted + dpa1_weighted + 
  corridor1_weighted + slope_weighted + river1_weighted
terra::plot(conservation_suitability, col = rev(terrain.colors(100)))

terra::writeRaster(conservation_suitability, "conservation_suitability_utm.tif")

# Fill the NA cells with the mean value
summary(terra::values(conservation_suitability))

conservation2 <- terra::focal(
  conservation_suitability, w = 9, fun = mean, na.policy = "only", na.rm = TRUE
)
summary(terra::values(conservation2))
terra::plot(conservation2)

terra::writeRaster(conservation2, "conservation_suitability_utm_121024.tif")


################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################

# PRESSURE

slope <- terra::rast("slope_utm_01.tif")

# Load raster files
urban <- terra::rast("distance_to_urban_centres_utm_01.tif")
fire <- terra::rast("fire_severity_utm_01.tif")
pop_den <- terra::rast("population_density_utm_01.tif")
pres_ind <- terra::rast("Pressure_index_01.tif")
pop_size <- terra::rast("population_size_utm_01.tif")
droad <- terra::rast("Distance_to_road_01_2.tif")

# Inspect extents
terra::ext(urban)
terra::ext(fire)
terra::ext(pop_den)
terra::ext(pres_ind)
terra::ext(pop_size)
terra::ext(droad)

# Investigate CRS
terra::crs(urban)
terra::crs(fire)
terra::crs(pop_den)
terra::crs(pres_ind)
terra::crs(pop_size)
terra::crs(droad)

# Extents don't match
# Resample to match extents
urban1 <- terra::resample(
  x = urban,
  y = slope
)

fire1 <- terra::resample(
  x = fire,
  y = slope
)

pop_den1 <- terra::resample(
  x = pop_den,
  y = slope
)

pres_ind1 <- terra::resample(
  x = pres_ind,
  y = slope
)

pop_size1 <- terra::resample(
  x = pop_size,
  y = slope
)

droad1 <- terra::resample(
  x = droad,
  y = slope
)

# Inspect extents after resampling
terra::ext(urban1)
terra::ext(fire1)
terra::ext(pop_den1)
terra::ext(pres_ind1)
terra::ext(pop_size1)
terra::ext(droad1)

# Ensure they can be stacked (only possible when all extents match)
all <- c(urban1, fire1, pop_den1, pres_ind1, pop_size1, droad1)
terra::plot(all)

# Rasters don't have proper names, so assign appropriate names
names(urban1) <- "urban"
names(fire1) <- "fire"
names(pop_den1) <- "pop_den"
names(pres_ind1) <- "pres_ind"
names(pop_size1) <- "pop_size"
names(droad1) <- "droad"

# Now stacked again and check names
all <- c(urban1, fire1, pop_den1, pres_ind1, pop_size1, droad1)
terra::plot(all) # seems all good

# check resolution
terra::res(all)

# Save resampled raster
terra::writeRaster(urban1, "distance_to_urban_centres_utm_resamp.tif")
terra::writeRaster(fire1, "fire_severity_utm_resamp.tif")
terra::writeRaster(pop_den1, "population_density_utm_resamp.tif")
terra::writeRaster(pres_ind1, "Pressure_index_resamp.tif")
terra::writeRaster(pop_size1, "population_size_utm_resamp.tif")
terra::writeRaster(droad1, "distance_to_road_utm_resamp.tif")

################################################################################
################################################################################
################################################################################


# Calculate suitability raster 

urb_wt <- eigenvector$Urban[5]
urban1_weighted <- urban1 * urb_wt

fire_wt <- eigenvector$Fire[5]
fire1_weighted <- fire1 * fire_wt

pop_den_wt <- eigenvector$`Pop-Den`[5]
pop_den1_weighted <- pop_den1 * pop_den_wt

pop_size_wt <- eigenvector$Pop_Size[5]
pop_size1_weighted <- pop_size1 * pop_size_wt

droad_wt <- eigenvector$DRoad[5]
droad1_weighted <- droad1 * droad_wt

pressure_map <- urban1_weighted + fire1_weighted + pop_den1_weighted + 
  pop_size1_weighted + droad1_weighted
terra::plot(pressure_map, col = rev(terrain.colors(100)))

terra::writeRaster(pressure_map, "pressure_map_utm.tif")

# Fill the NA cells with the mean value
summary(terra::values(pressure_map))

pressure2 <- terra::focal(
  pressure_map, w = 9, fun = mean, na.policy = "only", na.rm = TRUE
)
summary(terra::values(pressure2))
terra::plot(pressure2)

terra::writeRaster(pressure2, "pressure_map_utm_181024.tif")

################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################


# BIVARIATE

# The function that produces the colour matrix
colmat <- function(
    nbreaks = 3, 
    breakstyle = "quantile",
    upperleft = rgb(0, 150, 235, maxColorValue = 255),
    upperright = rgb(130, 0, 80, maxColorValue = 255),
    bottomleft = "#cccccc",
    bottomright = rgb(255, 230, 15, maxColorValue = 255),
    xlab = "x label", 
    ylab = "y label", 
    plotLeg = TRUE,
    saveLeg = FALSE
) {
  # TODO - replace any tidyr, dplyr etc. functions with data.table #
  library(tidyverse)
  require(ggplot2)
  require(classInt)
  library(dplyr)
  if (breakstyle == "sd") {
    warning(
      "SD breaks style cannot be used.\nWill not always return the correct number of breaks.\nSee classInt::classIntervals() for details.\nResetting to quantile",
      call. = FALSE, 
      immediate. = FALSE
    )
    breakstyle <- "quantile"}
  # The colours can be changed by changing the HEX codes for:
  # upperleft, upperright, bottomleft, bottomright
  # From http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  # upperleft = "#64ACBE", upperright = "#574249", bottomleft = "#E8E8E8", bottomright = "#C85A5A",
  # upperleft = "#BE64AC", upperright = "#3B4994", bottomleft = "#E8E8E8", bottomright = "#5AC8C8",
  # upperleft = "#73AE80", upperright = "#2A5A5B", bottomleft = "#E8E8E8", bottomright = "#6C83B5", 
  # upperleft = "#9972AF", upperright = "#804D36", bottomleft = "#E8E8E8", bottomright = "#C8B35A",
  # upperleft = "#DA8DC8", upperright = "#697AA2", bottomleft = "#E8E8E8", bottomright = "#73BCA0",
  # Similar to Teuling, Stockli, Seneviratnea (2011) [https://doi.org/10.1002/joc.2153]
  # upperleft = "#F7900A", upperright = "#993A65", bottomleft = "#44B360", bottomright = "#3A88B5",
  # Viridis style
  # upperleft = "#FEF287", upperright = "#21908D", bottomleft = "#E8F4F3", bottomright = "#9874A1",
  # Similar to Fjeldsa, Bowie, Rahbek 2012
  # upperleft = "#34C21B", upperright = "#FFFFFF", bottomleft = "#595757",  bottomright = "#A874B8",
  # Default from original source
  # upperleft = "#0096EB", upperright = "#820050", bottomleft= "#BEBEBE", bottomright = "#FFE60F",
  my.data <- seq(0, 1, .01)
  # Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
  my.class <- classInt::classIntervals(
    my.data,
    n = nbreaks,
    style = breakstyle,
  )
  my.pal.1 <- classInt::findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- classInt::findColours(my.class, my.col)
  }
  ## need to convert this to data.table at some stage.
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>% 
    mutate("Y" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>% 
    pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>% 
    mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(Y = rev(.$Y)) %>% 
    dplyr::select(-c(4)) %>%
    mutate("Y" = rep(seq(from = 1, to = nbreaks, by = 1), each = nbreaks),
           "X" = rep(seq(from = 1, to = nbreaks, by = 1), times = nbreaks)) %>%
    mutate("UID" = row_number())
  # Use plotLeg if you want a preview of the legend
  if (plotLeg) {
    p <- ggplot(col.matrix.plot, aes(X, Y, fill = HEXCode)) +
      geom_tile() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(
        aspect.ratio = 1,
        axis.title = element_text(size = 12, colour = "black",hjust = 0.5, vjust = 1),
        axis.title.y = element_text(angle = 90, hjust = 0.5)
      ) +
      xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ylab(bquote(.(ylab) ~  symbol("\256")))
    print(p)
    assign(
      x = "BivLegend",
      value = p,
      pos = .GlobalEnv
    )
  }
  # Use saveLeg if you want to save a copy of the legend
  if (saveLeg) {
    ggsave(filename = "bivLegend.pdf", plot = p, device = "pdf",
           path = "./", width = 4, height = 4, units = "in",
           dpi = 300)
  }
  seqs <- seq(0, 100, (100 / nbreaks))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
  attr(col.matrix, "breakstyle") <- breakstyle
  attr(col.matrix, "nbreaks") <- nbreaks
  return(col.matrix)
}

# Function to assign colour-codes to raster data
# As before, by default assign tercile breaks
bivariate.map <- function(
    rasterx, 
    rastery, 
    colourmatrix = col.matrix,
    export.colour.matrix = TRUE,
    outname = paste0("colMatrix_rasValues", names(rasterx))
) {
  # TO DO - replace raster with terra #
  require(raster)
  require(classInt)
  # export.colour.matrix will export a data.frame of rastervalues and RGB codes 
  # to the global environment outname defines the name of the data.frame
  quanx <- getValues(rasterx)
  tempx <- data.frame(quanx, quantile = rep(NA, length(quanx)))
  brks <- with(
    tempx, 
    classIntervals(
      quanx,
      n = attr(colourmatrix, "nbreaks"),
      style = attr(colourmatrix, "breakstyle"))$brks
  )
  ## Add (very) small amount of noise to all but the first break
  ## https://stackoverflow.com/a/19846365/1710632
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r1 <- within(
    tempx, 
    quantile <- cut(
      quanx,
      breaks = brks,
      labels = 2:length(brks),
      include.lowest = TRUE
    )
  )
  quantr <- data.frame(r1[, 2])
  quany <- getValues(rastery)
  tempy <- data.frame(quany, quantile = rep(NA, length(quany)))
  brksy <- with(
    tempy, 
    classIntervals(
      quany,
      n = attr(colourmatrix, "nbreaks"),
      style = attr(colourmatrix, "breakstyle"))$brks
  )
  brksy[-1] <- brksy[-1] + seq_along(brksy[-1]) * .Machine$double.eps
  r2 <- within(
    tempy, 
    quantile <- cut(
      quany,
      breaks = brksy,
      labels = 2:length(brksy),
      include.lowest = TRUE
    )
  )
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <- colourmatrix
  cn <- unique(colourmatrix)
  for (i in 1:length(col.matrix2)) {
    ifelse(
      is.na(col.matrix2[i]),
      col.matrix2[i] <- 1,
      col.matrix2[i] <- which(col.matrix2[i] == cn)[1]
    )
  }
  # Export the colour.matrix to data.frame() in the global env
  # Can then save with write.table() and use in ArcMap/QGIS
  # Need to save the output raster as integer data-type
  if (export.colour.matrix) {
    # create a dataframe of colours corresponding to raster values
    exportCols <- as.data.frame(cbind(
      as.vector(col.matrix2), as.vector(colourmatrix),
      t(col2rgb(as.vector(colourmatrix)))
    ))
    # rename columns of data.frame()
    colnames(exportCols)[1:2] <- c("rasValue", "HEX")
    # Export to the global environment
    assign(
      x = outname,
      value = exportCols,
      pos = .GlobalEnv
    )
  }
  cols <- numeric(length(quantr[, 1]))
  for (i in 1:length(quantr[, 1])) {
    a <- as.numeric.factor(quantr[i, 1])
    b <- as.numeric.factor(quantr2[i, 1])
    cols[i] <- as.numeric(col.matrix2[b, a])
  }
  r <- rasterx
  r[1:length(r)] <- cols
  return(r)
}

################################################################################
################################################################################

# Apply the function

################################################################################
################################################################################

# Define the number of breaks
nBreaks <- 59

# Create the colour matrix
col.matrixQ <- colmat(
  nbreaks = nBreaks, 
  breakstyle = "quantile",
  xlab = "Conservation", ylab = "Pressure", 
  upperleft = rgb(0, 150, 235, maxColorValue = 255),
  upperright = rgb(130, 0, 80, maxColorValue = 255),
  bottomleft = "#cccccc",
  bottomright = rgb(255, 230, 15, maxColorValue = 255),
  saveLeg = FALSE,
  plotLeg = TRUE
)

################################################################################

terra::res(conservation2); terra::res(pressure2)
# 10m resolution, this will take a long time to process
# For fast computation, make the pixels coarse

conservation <- terra::aggregate(conservation2, fact = 50) # 10m * 50 = 500m
terra::plot(conservation)

pressure <- terra::aggregate(pressure2, fact = 50)
terra::plot(pressure)

################################################################################

# create the bivariate raster
bivmapQ <- bivariate.map(
  rasterx = raster::raster(conservation), 
  rastery = raster::raster(pressure),
  export.colour.matrix = FALSE,
  colourmatrix = col.matrixQ
)

# Convert to dataframe for plotting with ggplot
bivMapDFQ <- data.table::setDT(as.data.frame(bivmapQ, xy = TRUE))
colnames(bivMapDFQ)[3] <- "BivValue"
bivMapDFQ <- reshape2::melt(
  bivMapDFQ, 
  id.vars = c("x", "y"),
  measure.vars = "BivValue",
  value.name = "bivVal",
  variable.name = "Variable"
)
str(bivMapDFQ)
hist(bivMapDFQ$bivVal, breaks = 20)

################################################################################

# Remove NAs
bivMapDFQ_noNA <- bivMapDFQ[!is.na(bivMapDFQ$bivVal), ]

################################################################################

################################################################################
################################################################################
################################################################################

# Make the map

map_q <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_fill_gradientn(
    colours = col.matrixQ, 
    na.value = "transparent",
    guide = "none" # removes legend
  ) + 
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::theme(
    legend.title = ggplot2::element_text(hjust = 0.15),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank()
  ) +
  ggplot2::coord_sf() +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.2,
    pad_x = ggplot2::unit(0.5, "cm"),
    pad_y = ggplot2::unit(0.5, "cm")
  ) +
  ggspatial::annotation_north_arrow(
    location = "bl",
    pad_x = ggplot2::unit(0.95, "cm"),
    pad_y = ggplot2::unit(0.75, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  theme_void()
map_q

map_q + ggtitle("") + 
  patchwork::inset_element(
    BivLegend + 
      theme(
        plot.background = element_rect(fill = "white", colour = NA)
      ), 
    left = -0.55, 
    bottom = 0.65, 
    right = 2.25, 
    top = 0.90,
    align_to = "full"
  ) +
  patchwork::plot_annotation(caption = "")


# Save
ggplot2::ggsave(
  'Nicaragua_MCDA_bivariate1.png',
  plot = ggplot2::last_plot(), # or fig
  width = 11,
  height = 7,
  dpi = 300
)

################################################################################
################################################################################
################################################################################

# Get elevation (DEM) for the aoi
aoi_dem <- elevatr::get_elev_raster(
  locations = conservation,
  #prj = sf::st_crs(shp_wgs),
  z = 8, # 1 to 14; larger numbers produce finer pixel resolution
  clip = "tile" #"bbox"
)
raster::plot(aoi_dem)

# Create hillshade
slope <- terra::terrain(terra::rast(aoi_dem), "slope", unit="radians")
aspect <- terra::terrain(terra::rast(aoi_dem), "aspect", unit="radians")

hill <- terra::shade(slope, aspect, 45, 270)

terra::plot(hill, col = grey(0:100/100), legend = FALSE, mar = c(2, 2, 1, 4))
terra::plot(
  terra::rast(aoi_dem), 
  col = terrain.colors(25, alpha = 0.35), 
  add = TRUE, 
  main = ""
)

################################################################################
################################################################################
################################################################################

# Multi-directional hillshade

# FINAL - works well

# pass multiple directions to shade()
hillmulti <- map(c(270, 15, 60, 330), function(dir){ 
  terra::shade(slope, aspect, 
               angle = 45, 
               direction = dir,
               normalize= TRUE)}
)

# create a multidimensional raster and reduce it by summing up
hillmulti <- terra::rast(hillmulti) |> sum()

# multidirectional
terra::plot(hillmulti, col = grey(1:100/100))


hillmulti_clip <- terra::crop(hillmulti, terra::ext(conservation))
hillmulti_mask <- terra::mask(hillmulti_clip, terra::vect(shp_wgs))
terra::plot(hillmulti_mask, col = grey(0:100/100), legend = FALSE, mar = c(2, 2, 1, 4))


# convert the hillshade to xyz
hillmulti_mask_prj <- terra::project(hillmulti_mask, terra::crs(bivmapQ))

hillmultidf <- as.data.frame(hillmulti_mask_prj, xy = TRUE)
str(hillmultidf)

map_q1 <- ggplot() +
  # geom_raster(data = hillmultidf, aes(x = x, y = y, alpha = sum), show.legend = FALSE) +
  # scale_fill_distiller(palette = "Greys", na.value = "transparent") +
  # scale_alpha(guide = "none") +
  # ggnewscale::new_scale_fill() + 
  geom_raster(
    #data = bivMapDFQ, 
    data = bivMapDFQ_noNA,
    mapping = aes(x = x, y = y, fill = bivVal),
    alpha = 1,
    na.rm = TRUE
  ) +
  scale_fill_gradientn(
    colours = col.matrixQ, 
    na.value = "transparent",
    guide = "none" # removes legend
  ) + 
  # ggplot2::geom_sf(
  #   data = sf::st_as_sf(shp_prj),
  #   inherit.aes = FALSE,
  #   color = "grey50",
  #   fill = NA,
  #   linewidth = 1
  # ) +
  # ggplot2::geom_sf(
  #   data = settle_crop,
  #   inherit.aes = FALSE,
  #   ggplot2::aes(colour = "A"),
  #   #colour = "purple",
  #   show.legend = "point"
  # ) +
  # ggplot2::geom_sf(
  #   data = road_crop,
  #   inherit.aes = FALSE,
  #   ggplot2::aes(colour = "B"),
  #   #colour = "grey25",
  #   alpha = 0.5,
  #   linewidth = 0.75,
  #   linetype = "solid",
  #   show.legend = "line"
  # ) +
  # ggplot2::geom_sf(
  #   data = river_crop,
  #   inherit.aes = FALSE,
  #   ggplot2::aes(colour = "C"),
  #   #colour = "royalblue",
  #   alpha = 0.35,
  #   linewidth = 0.85,
  #   show.legend = "line"
  # ) +
  # ggplot2::scale_colour_manual(
  #   values = c("A" = "red", "B" = "grey25", "C" = "dodgerblue"),
  #   labels = c("Settlement", "Road", "River"),
  #   name = "",
  #   guide = ggplot2::guide_legend(
  #     override.aes = list(
  #       linetype = c("blank","solid", "solid"),
  #       shape = c(16, NA, NA))
  #   )
  # ) +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude") +
  ggplot2::theme(
    legend.title = ggplot2::element_text(hjust = 0.15),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank()
  ) +
  ggplot2::coord_sf() +
  ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.2,
    pad_x = ggplot2::unit(0.5, "cm"),
    pad_y = ggplot2::unit(0.5, "cm")
  ) +
  ggspatial::annotation_north_arrow(
    location = "br",
    pad_x = ggplot2::unit(0.95, "cm"),
    pad_y = ggplot2::unit(0.75, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  theme_bw()
map_q1

map_q1 + ggtitle("") + 
  patchwork::inset_element(
    BivLegend + 
      theme(
        plot.background = element_rect(fill = "white", colour = NA)
      ), 
    left = -0.55, 
    bottom = 0.65, 
    right = 0.85, 
    top = 0.90,
    align_to = "full"
  ) +
  patchwork::plot_annotation(caption = "")


# Save
ggplot2::ggsave(
  'Sapo_chimp_occupancy_abundance_bivariate_full_hillshade_nobg.png',
  plot = ggplot2::last_plot(),
  width = 11,
  height = 7,
  dpi = 300
)

################################################################################
################################################################################
################################################################################


###********** NEWER VERSION OF BIVARIATE MAPPING SCRIPT 11-10-2024 **********###


# pass multiple directions to shade()
hillmulti <- purrr::map(c(270, 15, 60, 330), function(dir){ 
  terra::shade(slope, aspect, 
               angle = 45, 
               direction = dir,
               normalize= TRUE)}
)

# create a multidimensional raster and reduce it by summing up
hillmulti <- terra::rast(hillmulti) |> sum()

# multidirectional
terra::plot(hillmulti, col = grey(1:100/100))

hillmulti <- terra::resample(hillmulti, conservation)

hillmulti_clip <- terra::crop(hillmulti, terra::ext(conservation))
hillmulti_mask <- terra::mask(hillmulti_clip, terra::ext(conservation))
terra::plot(hillmulti_mask, col = grey(0:100/100), legend = FALSE, mar = c(2, 2, 1, 4))

# convert the hillshade to xyz
hillmulti_mask_prj <- terra::project(hillmulti_mask, terra::crs(conservation))

hillmultidf <- as.data.frame(hillmulti_mask_prj, xy = TRUE)
str(hillmultidf)

###**************************************************************************###

# Stack rasters
two_ras <- c(conservation, pressure)
names(two_ras) <- c("Conservation", "Pressure")
terra::plot(two_ras)

# COnvert to data frame
two_ras_df <- two_ras |> 
  terra::project(conservation) |> 
  as.data.frame(xy = TRUE)
head(two_ras_df)

# Bivaraite mapping
data <- biscale::bi_class(
  two_ras_df,
  x = Conservation, 
  y = Pressure, 
  style = "quantile", 
  dim = 4
)
str(data)

###**************************************************************************###

# Colour palette
#c_pal <- "BlueOr"
#c_pal <- "PurpleGrn"
c_pal <- "DkViolet2"
#c_pal <- "PinkGrn"
#c_pal <- "PurpleOr"
#c_pal <- "DkBlue2"

# Create the bivariate map using ggplot2
map <- ggplot2::ggplot() +
  ggplot2::theme_void(base_size = 14) +  # Set a minimal theme for the map
  ggplot2::geom_raster(
    data = data, 
    mapping = ggplot2::aes(x = x, y = y, fill = bi_class), 
    show.legend = FALSE
  ) +
  # Apply the bivariate color scale using the selected palette and dimensions
  biscale::bi_scale_fill(
    pal = c_pal, 
    dim = 4, 
    flip_axes = FALSE, 
    rotate_pal = FALSE
  ) +
  ggplot2::coord_sf() +
  # Add labels for the map
  ggplot2::labs(title = "Conservation and Human Pressure Patterns", 
                subtitle = "",
                caption = "") +
  # Customize the appearance of the title, subtitle, and caption
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10, face = "bold", hjust = 7)
  )


# Create the legend for the bivariate map
legend <- biscale::bi_legend(
  pal = c_pal,   
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = 4,
  xlab = "Conservation",
  ylab = "Human Pressure",
  size = 10
)

# Combine the map and legend using cowplot

finalPlot <- cowplot::ggdraw() +
  cowplot::draw_plot(map, 0, 0, 1, 1) +  # Draw the main map plot
  cowplot::draw_plot(legend, 0.05, 0.05, 0.28, 0.28)  # Draw the legend in the specified position

# Display the final map with legend
finalPlot

# Saving the Map
ggsave("Nicaragua_MCDA_Cons_Pres.png", finalPlot, dpi = 400, width = 7, height = 7)

###**************************************************************************###
###**************************************************************************###
###**************************************************************************###
###**************************************************************************###
###**************************************************************************###

# Fill the NA cells with the mean value

shp <- sf::st_read("study_region_provided.shp")
shp_prj <- sf::st_transform(shp, sf::st_crs(conservation))

summary(terra::values(conservation))

conservation2 <- terra::focal(
  conservation, w = 9, fun = mean, na.policy = "only", na.rm = TRUE
)
summary(terra::values(conservation2))
terra::plot(conservation2)

conservation2 <- terra::crop(conservation2, shp_prj)
conservation2 <- terra::mask(conservation2, shp_prj)
terra::plot(conservation2)

summary(terra::values(pressure))

pressure2 <- terra::focal(
  pressure, w = 9, fun = mean, na.policy = "only", na.rm = TRUE
)
summary(terra::values(pressure2))
terra::plot(pressure2)

pressure2 <- terra::crop(pressure2, shp_prj)
pressure2 <- terra::mask(pressure2, shp_prj)
terra::plot(pressure2)


# Fine scale
cons <- terra::disagg(conservation2, fact = 10)
pres <- terra::disagg(pressure2, fact = 10)

# Stack rasters
two_ras <- c(pres, cons)
names(two_ras) <- c("Pressure", "Conservation")
terra::plot(two_ras)

# COnvert to data frame
two_ras_df <- two_ras |> 
  terra::project(conservation2) |> 
  as.data.frame(xy = TRUE)
head(two_ras_df)

# Bivaraite mapping
data <- biscale::bi_class(
  two_ras_df,
  x = Pressure, 
  y = Conservation, 
  style = "quantile", 
  dim = 4
)
str(data)

c_pal <- "DkViolet2"

map <- ggplot2::ggplot() +
  ggplot2::theme_void(base_size = 14) +  # Set a minimal theme for the map
  ggplot2::geom_raster(
    data = data, 
    mapping = ggplot2::aes(x = x, y = y, fill = bi_class), 
    show.legend = FALSE
  ) +
  # Apply the bivariate color scale using the selected palette and dimensions
  biscale::bi_scale_fill(
    pal = custom_pal4, 
    dim = 4, 
    flip_axes = FALSE, 
    rotate_pal = FALSE
  ) +
  # Add labels for the map
  ggplot2::labs(title = "Conservation and Human Pressure Patterns", 
                subtitle = "",
                caption = "") +
  ggplot2::coord_sf() +
  # Customize the appearance of the title, subtitle, and caption
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 10, face = "bold", hjust = 7)
  )



# Create the legend for the bivariate map
legend <- biscale::bi_legend(
  pal = c_pal,   
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = 4,
  xlab = "Human Pressure",
  ylab = "Conservation",
  size = 10
)

# Combine the map and legend using cowplot

finalPlot <- cowplot::ggdraw() +
  cowplot::draw_plot(map, 0, 0, 1, 1) +  # Draw the main map plot
  cowplot::draw_plot(legend, 0.05, 0.05, 0.28, 0.28)  # Draw the legend in the specified position

# Display the final map with legend
finalPlot

# Saving the Map
ggsave("Nicaragua_MCDA_Cons_Pres.png", finalPlot, dpi = 400, width = 7, height = 7)

################################################################################
################################################################################
################################################################################
