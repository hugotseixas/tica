# HEADER ----------------------------------------------------------------------
#
# Title:          Create project grid
# Description:    This script creates a grid that will be used for all
#                 data in this project
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(sf)
library(geobr)
library(conflicted)
library(tidyverse)
#
# CONFLICTS -------------------------------------------------------------------
#

conflicts_prefer(dplyr::filter)

#
# OPTIONS ---------------------------------------------------------------------
#

# Size of the grid cell in degrees
resolution <- 0.2

# Create grid with cells that are fully within the biomes?
full_cells <- TRUE
# If TRUE, st_within is called as predicate for spatial filter
# If FALSE, st_intersects is called as predicate for spatial filter

#
# LOAD DATA -------------------------------------------------------------------

# Load biomes limit
biomes <-
  read_biomes(
    year = 2019,
    simplified = TRUE, # Biome limits are simplified (topology preserved)
    showProgress = FALSE
  ) %>%
  filter(name_biome %in% c("Amazônia", "Cerrado"))

# CREATE AND SAVE GRID --------------------------------------------------------

# Create grid based on biomes polygons
grid <-
  st_make_grid(
    x = biomes,
    cellsize = resolution # Resolution of the grid cell (check options above)
  ) %>%
  st_as_sf() %>%
  st_filter(
    y = biomes,
    # Choose filter function based on option above
    .predicate = if (full_cells) {st_within} else {st_intersects}
  ) %>%
  rename(cell = x) %>%
  mutate(id = row_number())

# Save grid polygons as FlatGeobuf file
write_sf(
  obj = grid,
  dsn = "./data/base_grid.fgb",
  driver = "FlatGeobuf",
  append = FALSE,
  delete_dsn = TRUE
)

# CREATE AND SAVE PLOT --------------------------------------------------------

# Plot grid and biome limits
grid_plot <- ggplot() +
  geom_sf(
    data = biomes,
    fill = "transparent"
  ) +
  geom_sf(
    data = grid,
    fill = "transparent"
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = NA))

# Save plot
ggsave(
  filename = "manuscript/figs/grid_cells.png",
  plot = grid_plot,
  device = ragg::agg_png,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 300
)
