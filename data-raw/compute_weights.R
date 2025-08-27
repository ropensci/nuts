# COMPUTE CONVERSION WEIGHTS

rm(list = ls())
library(tidyverse)
library(devtools)
library(sf)
library(janitor)
library(terra)
library(cli)


level = '3'
version = '2003'

compute_weights = function(level, version){

cli_alter('Computing weight for level {level} and version {level}')

file_intersections <- paste0("data-raw/own-conversion-matrices/intersections_lev_",
               level, "_vers_", version, ".rda")

load(file = file_intersections)

pop <- rast("C:/Users/Hennicke/Downloads/JRC_GRID_2018/JRC_1K_POP_2018.tif")

shape_inter$pop18 <- extract(pop, shape_inter, fun = 'sum')[,2] %>%
  st_set_geometry(NULL)

return(shape_inter)
}


cross_walks <- compute_weights('2', '2006')


cross_walks
