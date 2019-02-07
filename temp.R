library(rayshader)
library(rgl)
library(magick)

# montery water gif ====
elevation_matrix <- montereybay
n_frames <- 180
zscale <- 50
waterdepthvalues = min(elevation_matrix)/2 - min(elevation_matrix)/2 * cos(seq(0,2*pi,length.out = n_frames))
thetavalues = -90 + 45 * cos(seq(0, 2*pi, length.out = n_frames))
ambmat = ambient_shade(elevation_matrix, zscale = zscale)
raymat = ray_shade(elevation_matrix, zscale = zscale, lambert = TRUE)

# generate gif images
for (i in 1:n_frames) {
  print(glue::glue("image {i} of {n}", i = i, n = n_frames))
  elevation_matrix %>%
    sphere_shade(texture = "imhof1", zscale = 10) %>%
    add_shadow(ambmat, 0.5) %>%
    add_shadow(raymat, 0.5) %>%
    plot_3d(elevation_matrix,
            solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
            waterdepth = waterdepthvalues[i]/zscale, watercolor = "imhof3", wateralpha = 0.8,
            waterlinecolor = "#ffffff", waterlinealpha = 0.5, waterlinewidth = 2,
            theta = thetavalues[i], phi = 45)
  rgl::snapshot3d(paste0("drain",i,".png"))
  rgl::clear3d()
}

# build gif
gif_file <- "drain.gif"
img_frames <- paste0("drain", seq(n_frames), ".png")
magick::image_write_gif(magick::image_read(img_frames), 
                        path = gif_file, delay = 6/n_frames)



# monterey ====
montereybay
dim(montereybay)
class(montereybay)

ambmat <- ambient_shade(elevation_matrix, zscale = zscale)
raymat <- ray_shade(elevation_matrix, zscale = zscale, lambert = TRUE)
montereybay %>%
  sphere_shade(zscale = 10, texture = "imhof1") %>%
  add_shadow(ambmat, 0.5) %>%
  add_shadow(raymat, 0.5) %>%
  plot_3d(montereybay,
    zscale = 50, fov = 0, theta = -45, phi = 45, 
    windowsize = c(1000, 800), zoom = 0.75,
    water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
    waterlinecolor = "white", waterlinealpha = 0.3
  )


# san francisco ====
local_img <- raster::raster(file.path("data", "sanfran.tiff"))
sf_sat <-  png::readPNG(file.path("data", "sf-World_Imagery.png"))
sf_map <-  png::readPNG(file.path("data", "sf-World_Topo_Map.png"))

# sf_map <-  png::readPNG(file.path("data", "nasa-sf.png"))
# image_read(sf_map)
# # trim image
# d <- dim(sf_map)
# bbox_ratio
# remove_n <- round(d[1] * (1 - bbox_ratio))
# keep_n <- d[1] - remove_n
# rows <- seq(1, d[1])
# keep_rows <- head(rows, -floor(remove_n/2)) %>% tail(keep_n)
# sf_map <- sf_map[keep_rows,,]
# image_read(sf_map)
# sf_map_resize <- image_resize(image_read(sf_map), "600x480!")
# image_write(sf_map_resize, path = "temp.png")
# sf_map <- png::readPNG("temp.png")

elmat <- matrix(
  raster::extract(local_img, raster::extent(local_img), buffer = 1000), 
  nrow = ncol(local_img), ncol = nrow(local_img)
)
dim(elmat)
# xmax <- ncol(elmat)
# ymax <- nrow(elmat)
# xrange <- floor(c(0.47, 0.65) * xmax)
# yrange <- floor(c(0.18, 0.3) * ymax)

elmat_sub <- elmat
# elmat_sub <- elmat[xrange[1]:xrange[2], yrange[1]:yrange[2]]
# dim(elmat_sub)

ambmat <- ambient_shade(elmat_sub, zscale=30)
raymat <- ray_shade(elmat_sub, zscale=30, lambert = TRUE)

# 2D
elmat_sub %>%
  sphere_shade(texture = "imhof4", sunangle = 315) %>%
  add_water(detect_water(elmat_sub), color = "desert") %>%
  add_shadow(raymat, 0.5) %>%
  add_shadow(ambmat, 0.5) %>%
  add_overlay(sf_map, alphalayer = 0.6) %>%
  plot_map()


# 3D
zscale <- 20  # 30 is correct
rgl::clear3d()
elmat_sub %>% 
  sphere_shade(zscale = zscale, texture = "imhof1") %>% 
  add_water(detect_water(elmat_sub), color = "imhof1") %>%
  add_overlay(sf_map, alphalayer = 0.6) %>%
  add_shadow(raymat, 0.5) %>%
  add_shadow(ambmat, 0.5) %>%
  plot_3d(elmat_sub,
    zscale = zscale, fov = 0, theta = -45, phi = 45,
    windowsize = c(1000, 800), zoom = 0.75, soliddepth = -100 / 30,
    water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
    waterlinecolor = "white", waterlinealpha = 0.3
  )


# REST API elevation ==== 
# https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage?bbox=-122.597807%2C37.688743%2C-122.323149%2C37.851575&bboxSR=4326&size=400%2C400&imageSR=4326&time=&format=jpgpng&pixelType=F32&noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=&mosaicRule=&renderingRule=&f=html
url <- "https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage?bbox=-122.597807%2C37.688743%2C-122.323149%2C37.851575&bboxSR=4326&size=400%2C400&imageSR=4326&time=&format=tiff&pixelType=F32&noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=&mosaicRule=&renderingRule=&f=image"

# map image
# json: {"mapOptions":{"extent":{"xmin":-122.525,"ymin":37.677,"xmax":-122.355,"ymax":37.813,"spatialReference":{"wkid":4326}}},"operationalLayers":[],"baseMap":{"title":"World_Terrain_Base","baseMapLayers":[{"url":"https://services.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer"}]},"exportOptions":{"outputSize":[400,320]},"layoutOptions":{"titleText":"City Land Use Map","authorText":"Print by: XYZ","copyrightText":"Bishop-maps","scaleBarOptions":{"metricUnit":"esriKilometers","metricLabel":"km","nonMetricUnit":"esriMiles","nonMetricLabel":"mi"}}}
# url: https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export Web Map Task/execute?f=json&Format=PNG32&Layout_Template=MAP_ONLY&Web_Map_as_JSON={"mapOptions":{"extent":{"xmin":-122.597807,"ymin":37.688743,"xmax":-122.323149,"ymax":37.851575,"spatialReference":{"wkid":4326}}},"operationalLayers":[],"baseMap":{"title":"World_Terrain_Base","baseMapLayers":[{"url":"https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer"}]},"exportOptions":{"outputSize":[800,800]},"layoutOptions":{"titleText":"City Land Use Map","authorText":"Print by: XYZ","copyrightText":"Bishop-maps","scaleBarOptions":{"metricUnit":"esriKilometers","metricLabel":"km","nonMetricUnit":"esriMiles","nonMetricLabel":"mi"}}}
# web-ui: https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute
# https://services.arcgisonline.com/ArcGIS/rest/services

# leaflet =====
library(leaflet)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = -122.45, lat = 37.75, popup = "The birthplace of R")
m  # Print the map
leaflet() %>%
  addTiles() %>% 
  addMarkers(lng = center$long, lat = center$lat) %>% 
  addRectangles(
    lng1 = bbox$lng1, lat1 = bbox$lat1,
    lng2 = bbox$lng2, lat2 = bbox$lat2,
    fillColor = "transparent"
  ) %>%
  fitBounds(lng1 = bbox$lng1, lat1 = bbox$lat1,
            lng2 = bbox$lng2, lat2 = bbox$lat2)


# rlang experimenting ====
require(rayshader)
require(magick)
require(rgl)
require(gifski)
library(rlang)

ambmat <- ambient_shade(elev_matrix, zscale = zscale)
raymat <- ray_shade(elev_matrix, zscale = zscale, lambert = TRUE)
watermap <- detect_water(elev_matrix)

hillshade <- elev_matrix %>%
  sphere_shade(texture = palette, zscale = zscale) %>%
  add_water(watermap, color = palette) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_shadow(raymat, max_darken = 0.5)


n_frames <- 50
# theta <- transition_values(range = c(-45, -135), n = 10, one_way = FALSE)
theta <- transition_values(start = -45, end = -135, steps = n_frames, one_way = FALSE)
zscale <- transition_values(start = 10, end = 1, steps = n_frames, 
                            one_way = FALSE, type = "lin")

hillshade %>% 
  create_3d_gif(elev_matrix, file = "test.gif", duration = 5,
                theta = theta, zscale = zscale, water = TRUE)


# 3D plot with labels ====
# label
label_coords <- list(long = -122.451100, lat = 37.749010)
x_img <- round(img_width * (label_coords$long - min(bbox$p1$long, bbox$p2$long)) / abs(bbox$p1$long - bbox$p2$long))
y_img <- round(img_height * (label_coords$lat - min(bbox$p1$lat, bbox$p2$lat)) / abs(bbox$p1$lat - bbox$p2$lat))

zscale <- 10
rgl::clear3d()
palette <- "imhof4"
elev_matrix %>% 
  sphere_shade(zscale = zscale, texture = palette) %>% 
  add_water(watermap, color = palette) %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, 0.4) %>%
  add_shadow(ambmat, 0.4) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = TRUE, soliddepth = -max(elev_matrix)/zscale,
          theta = 25, phi = 30, zoom = 0.65, fov = 60)
render_snapshot()
# add label
render_label(elev_matrix, x = x_img, y = y_img, z = 500, zscale = zscale,
               text = "Emilia & Aaron!", textsize = 2, linewidth = 5)
render_snapshot("house-snap.png")




