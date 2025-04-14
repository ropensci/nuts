# COMPUTE CONVERSION WEIGHTS

rm(list = ls())
library(tidyverse)
library(devtools)
library(sf)
library(janitor)
library(terra)
library(cli)


# # Load all shapes
# shapes <- list.files("data-raw", pattern="*.shp", full.names=TRUE, recursive = TRUE)
#
# # Keep shapes with best resolution
# shapes <- shapes[grepl("01M", shapes)]
# #shapes <- shapes[1:3]
#
# # Get versions
# versions <- str_extract(shapes, "20\\d{2}" )
#
# # Read all shapes
# shapes <- map(shapes, \(x) read_sf(x))
#
# # Rename
# shapes <- map(shapes, \(x) x %>%
#   select(from_code = NUTS_ID, country = CNTR_CODE))
#
# # Add versions
# shapes <- map(1:length(versions), \(i) shapes[[i]]  %>%
#                 mutate(from_version = versions[i]))
#
# # Work only with selection
# shapes <- map(shapes, \(x) x %>%
#                 #filter(country %in% c("BE")) %>%
#                 st_make_valid())
#
# # Aggregate polygons at correct level
# shapes <- map(shapes, \(x) x %>%
#                 group_by(from_code, country, from_version) %>%
#                 summarise(geometry = st_union(geometry)) %>%
#                 ungroup() %>%
#                 st_make_valid())
#
# names(shapes) <- versions
#
# save(shapes, file = "data-raw/own-conversion-matrices/shapes.rda")


load("data-raw/own-conversion-matrices/shapes.rda")
versions <- names(shapes)

# Function to intersect shapes
intersect_for_nuts_level_version = function(shapes, level, version){

  # Focus on NUTS Level 3
  shapes_level <- map(shapes, \(x) x %>%
              mutate(level = level) %>%
              filter(nchar(from_code) == 2 + level))

  # Focus on version
  shapes_version <- shapes_level[[version]]

  # Loop over all versions
  shapes_inter <- map(shapes_level, \(shape){

    # Intersect shapes
    shapes_1_2 <- st_intersection(shapes_version, shape) %>%
      rename(to_code = from_code.1, to_version = from_version.1) %>%
      st_make_valid()

    # Filter out walks from one country to another
    shapes_1_2 <- shapes_1_2 %>%
      filter(country == country.1) %>%
      select(-country.1, -level.1)

    # Compute area
    shapes_1_2 <- shapes_1_2 %>%
      mutate(areaKm = as.numeric(st_area(geometry) / 1000000))

    # Keep only walks of more than 1 sqkm
    shapes_1_2 <- shapes_1_2 %>%
      filter(areaKm >= 1)

  }) %>%
    bind_rows()

  return(shapes_inter)

}



# Create grid to loop over
df_lev_vers <- expand.grid(levels = 1:3, versions = versions)
df_lev_vers

# Loop over all levels and versions
nr_loops <- nrow(df_lev_vers)

map(1:nr_loops, \(i) {

    level_i <- df_lev_vers$levels[i]
    version_i <- df_lev_vers$versions[i]
    file_path <- paste0('data-raw/own-conversion-matrices/intersections_lev_',
      level_i, '_vers_', version_i, '.rda')

    if (file.exists(file_path)) {
    cli_alert_success('Intersections created for level {level_i} and version {version_i}')
    } else {

      shape_inter <- intersect_for_nuts_level_version(
        shapes = shapes,
        level = level_i,
        version = version_i
      )

      save(shape_inter, file = file_path)
      cli_alert_success('Intersections created for level {level_i} and version {version_i}')
    }

})



# # Create grid to loop over
# df_lev_vers <- expand.grid(levels = 1:3, versions = versions)
#
# # Loop over all levels and versions
# nr_loops <- nrow(df_lev_vers)
#
# cross_walks_shapes <-
#   lapply(cli_progress_along(1:nr_loops, "Looping over all combinations"), function(i) {
#
#     df <- intersect_for_nuts_level_version(
#       shapes = shapes,
#       level = df_lev_vers$levels[i],
#       version = df_lev_vers$versions[i]
#     )
#
#   return(df)
#
# }) %>% bind_rows()
#
#
# save(cross_walks_shapes, file = "data-raw/own-conversion-matrices/cross_walks_shapes.rda")




# Save
# library(nuts)
# bla <- cross_walks_shapes %>%
#   st_set_geometry(NULL) %>%
#   group_by(from_version, to_version, level) %>%
#   tally()
# bla %>%
#   print(n = 100)
#
# blubb <- cross_walks %>%
#   arrange(from_version) %>%
#   group_by(from_version, to_version, level) %>%
#   tally()
#
# anti_join(blubb, bla, by = c("from_version", "to_version", "level" ))
# anti_join(bla, blubb, by = c("from_version", "to_version", "level" )) %>% print(n = 100)
