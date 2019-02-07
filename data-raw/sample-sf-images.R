library(httr)
library(glue)
library(jsonlite)
source(file.path("R", "map-image-api.R"))
source(file.path("R", "elevation-api.R"))
source(file.path("R", "image-size.R"))


# SF bounding box
bbox <- list(
  p1 = list(long = -122.522, lat = 37.707),
  p2 = list(long = -122.354, lat = 37.84) #37.84, 37.817
)
image_size <- define_image_size(bbox, major_dim = 400)


# download different map types
get_map_image(bbox, map_type = "World_Topo_Map", 
              file = file.path("images", "sf-topo.png"),
              width = image_size$width, 
              height = image_size$height)
get_map_image(bbox, map_type = "World_Imagery", 
              file = file.path("images", "sf-satellite.png"),
              width = image_size$width, 
              height = image_size$height)
get_map_image(bbox, map_type = "World_Street_Map", 
              file = file.path("images", "sf-street.png"),
              width = image_size$width, 
              height = image_size$height)

