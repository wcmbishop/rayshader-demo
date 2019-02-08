# Code samples presented in the radix web article
# on making your own custom rayshader plots.

library(httr)
library(glue)
library(jsonlite)
library(leaflet)
library(raster)
library(rayshader)

source(file.path("R", "map-image-api.R"))
source(file.path("R", "elevation-api.R"))
source(file.path("R", "image-size.R"))
source(file.path("R", "rayshader-gif.R"))
source(file.path("R", "read-elevation.R"))
source(file.path("R", "find-image-coordinates.R"))


# montererybay example --------------------------------------------------
montereybay %>%
  sphere_shade(zscale = 10, texture = "imhof1") %>%
  add_shadow(ray_shade(montereybay, zscale = 50)) %>%
  add_shadow(ambient_shade(montereybay, zscale = 50)) %>%
  plot_3d(montereybay, zscale = 50, theta = -45, phi = 45, water = TRUE,
          windowsize = c(1000,800), zoom = 0.75, waterlinealpha = 0.3,
          wateralpha = 0.5, watercolor = "lightblue", waterlinecolor = "white")
render_snapshot()


# selecting a map region --------------------------------------------------
# define bounding box with longitude/latitude coordinates
bbox <- list(
  p1 = list(long = -122.522, lat = 37.707),
  p2 = list(long = -122.354, lat = 37.84)
)
# display the bounding box
leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
  )
# define image size for map image files
image_size <- define_image_size(bbox, major_dim = 600)


# getting elevation data --------------------------------------------------
# download data from api
elev_file <- file.path("data", "sf-elevation.tif")
get_usgs_elevation_data(bbox, size = image_size$size, 
                        file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)
# load data into an elevation matrix
elev_matrix <- read_elevation_file(elev_file)


# getting map overlay image --------------------------------------------------
overlay_file <- "images/sf-map.png"
get_arcgis_map_image(bbox, map_type = "World_Topo_Map", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)
overlay_img <- png::readPNG(overlay_file)


# plot 2D --------------------------------------------------
# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)
watermap <- detect_water(elev_matrix)

# plot 2D
elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()


# plot 2D with overlay --------------------------------------------------
elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  plot_map()


# plot 3D with overlay --------------------------------------------------
zscale <- 10
rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = "imhof4") %>% 
  add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = TRUE, soliddepth = -max(elev_matrix)/zscale,
          theta = 25, phi = 30, zoom = 0.65, fov = 60)
render_snapshot()


# plot 3D with overlay and label --------------------------------------------------
# define label
label <- list(text = "Sutro Tower")
label$pos <- find_image_coordinates(
  long = -122.452131, lat = 37.756735, bbox = bbox,
  image_width = image_size$width, image_height = image_size$height)

# plot 3D
zscale <- 10
rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = "imhof4") %>% 
  add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = TRUE, soliddepth = -max(elev_matrix)/zscale, wateralpha = 0,
          theta = 25, phi = 30, zoom = 0.65, fov = 60)
# add label
render_label(elev_matrix, x = label$pos$x, y = label$pos$y, z = 500, 
             zscale = zscale, text = label$text, textsize = 2, linewidth = 5)
render_snapshot()


# montererybay gif --------------------------------------------------
# calculate input vectors for gif frames
n_frames <- 180
waterdepths <- transition_values(from = 0, to = min(montereybay), steps = n_frames) 
thetas <- transition_values(from = -45, to = -135, steps = n_frames)
# generate gif
zscale <- 50
montereybay %>% 
  sphere_shade(texture = "imhof1", zscale = zscale) %>%
  add_shadow(ambient_shade(montereybay, zscale = zscale), 0.5) %>%
  add_shadow(ray_shade(montereybay, zscale = zscale, lambert = TRUE), 0.5) %>%
  save_3d_gif(montereybay, file = "montereybay.gif", duration = 6,
              solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
              watercolor = "imhof3", wateralpha = 0.8, 
              waterlinecolor = "#ffffff", waterlinealpha = 0.5,
              waterdepth = waterdepths/zscale, 
              theta = thetas, phi = 45)


# san francisco fly-by gif --------------------------------------------------
# create transition variables
n_frames <- 180
theta <- transition_values(from = 0, to = 360, steps = n_frames, 
                           one_way = TRUE, type = "lin")
phi <- transition_values(from = 10, to = 70, steps = n_frames, 
                         one_way = FALSE, type = "cos")
zoom <- transition_values(from = 0.4, to = 0.8, steps = n_frames, 
                          one_way = FALSE, type = "cos")

# gif it!
zscale <- 10
elev_matrix %>% 
  sphere_shade(texture = "imhof4") %>% 
  add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, 0.4) %>%
  add_shadow(ambmat, 0.4) %>%
  save_3d_gif(elev_matrix, file = "images/sf-flyby.gif", duration = 6,
              zscale = zscale, windowsize = c(1200, 1000), wateralpha = 0,
              water = TRUE, soliddepth = -max(elev_matrix)/zscale,
              theta = theta, phi = phi, zoom = zoom, fov = 60)

