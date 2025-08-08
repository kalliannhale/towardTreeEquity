#################################################################
### BEAT 2: FINAL HEALTH CHECK
### Verify that all analysis-ready data is in tip-top shape.
#################################################################

cat("--- Running Final Health Checks for Beat 2 ---\n")

# --- Check 1: CRS Consistency ---
# All layers MUST be in the same Coordinate Reference System.
# We expect them all to be EPSG:2263.

cat("\n--- 1. CRS Consistency Check ---\n")
print(paste("Parks CRS:", sf::st_crs(parks)$input))
print(paste("Roads CRS:", sf::st_crs(roads)$input))
print(paste("City Boundary CRS:", sf::st_crs(city_boundary)$input))
print(paste("LST Raster CRS:", terra::crs(lst_raster, proj=TRUE)))
print(paste("Land Cover Raster CRS:", terra::crs(land_cover_raster, proj=TRUE)))
cat(" -> Desired Outcome: All CRS should be EPSG:2263 or its equivalent.\n")


# --- Check 2: Data Summaries & Sanity Check ---
# Do the objects contain reasonable data?

cat("\n--- 2. Data Summary & Sanity Check ---\n")
# How many features are left after cropping?
print(paste(nrow(parks), "park features are in the final study area."))
print(paste(nrow(roads), "road features are in the final study area."))

# What is the range of our LST raster? Should be in Celsius.
cat("\nLST (Celsius) value range:\n")
print(summary(values(lst_raster)))
cat(" -> Desired Outcome: A plausible range of temperatures for NYC (e.g., ~15-50 C).\n")

# What are the unique codes in our land cover raster?
# These are the numbers we will need to use in Beat 4.
cat("\nUnique Land Cover codes present in the final study area:\n")
print(sort(unique(values(land_cover_raster))))
cat(" -> Action Item: Note these numbers down for later use!\n")


# --- Check 3: The Ultimate Visual Check ---
# A picture is worth a thousand lines of code. Let's plot the layers on
# top of each other. If they are aligned, they will overlap perfectly.

cat("\n--- 3. Visual Alignment Check ---\n")
cat(" -> Generating plot... Check the 'Plots' pane in RStudio.\n")

# Set up plot parameters to prevent scientific notation on axes
options(scipen=999)

# Plot the LST raster as the base layer
plot(lst_raster, main = "Final Data Alignment Check")

# Add the other layers on top
plot(st_geometry(city_boundary), add = TRUE, border = "black", lwd = 2) # City outline in thick black
plot(st_geometry(parks), add = TRUE, border = "green", lwd = 1.5)      # Parks in green
plot(st_geometry(roads), add = TRUE, col = "grey40", lwd = 0.5)      # Roads in grey

cat(" -> Desired Outcome: In the plot, you should see parks (green) and roads (grey)\n")
cat("    fitting perfectly within the city boundary (black), all on top of the LST raster.\n")

# Reset plotting options
options(scipen=0)