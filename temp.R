library(rayshader)
library(rgl)
library(magick)

# montery water gif ====
elevation_matrix <- montereybay
n_frames <- 180
zscale <- 50
waterdepthvalues = min(elevation_matrix)/2 - min(elevation_matrix)/2 * cos(seq(0,2*pi,length.out = n_frames))
thetavalues = -90 + 45 * cos(seq(0,2*pi,length.out = n_frames))
ambmat = ambient_shade(elevation_matrix, zscale = zscale)
raymat = ray_shade(elevation_matrix, zscale = zscale, lambert = TRUE)

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
  rgl::snapshot3d(paste0("drain",i,".png"), top = TRUE)
  rgl::clear3d()
}

# build gif
gif_file <- "drain.gif"
img_frames <- paste0("drain", seq(n_frames), ".png")
image_write_gif(image_read(img_frames), path = gif_file, delay = 6/n_frames)
utils::browseURL(gif_file)



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
# overlay_img <-
local_img <- raster::raster(file.path("data", "sanfran.tiff"))
sf_map <-  png::readPNG(file.path("data", "sanfran-sat.png"))
# local_img <- raster::raster(file.path("data-raw", "n38w123", "imgn38w123_1.img"))
elmat <- matrix(
  raster::extract(local_img, raster::extent(local_img), buffer = 1000), 
  nrow = ncol(local_img), ncol = nrow(local_img)
)
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
  add_water(detect_water(elmat_sub), color="desert") %>%
  add_overlay(sf_map, alphalayer = 0.7) %>%
  add_shadow(raymat, 0.5) %>%
  add_shadow(ambmat, 0.5) %>%
  plot_map()


# 3D
zscale <- 20  # 30 is correct
rgl::clear3d()
elmat_sub %>% 
  sphere_shade(zscale=zscale, texture = "imhof1") %>% 
  add_water(detect_water(elmat_sub), color="imhof1") %>%
  add_overlay(sf_map, alphalayer = 0.7) %>%
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
# json: {"mapOptions":{"extent":{"xmin":-122.597807,"ymin":37.688743,"xmax":-122.323149,"ymax":37.851575,"spatialReference":{"wkid":4326}}},"operationalLayers":[],"baseMap":{"title":"World_Terrain_Base","baseMapLayers":[{"url":"https://services.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer"}]},"exportOptions":{"outputSize":[400,400]},"layoutOptions":{"titleText":"City Land Use Map","authorText":"Print by: XYZ","copyrightText":"Bishop-maps","scaleBarOptions":{"metricUnit":"esriKilometers","metricLabel":"km","nonMetricUnit":"esriMiles","nonMetricLabel":"mi"}}}
# url: https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export Web Map Task/execute?f=json&Format=PNG32&Layout_Template=MAP_ONLY&Web_Map_as_JSON={"mapOptions":{"extent":{"xmin":-122.597807,"ymin":37.688743,"xmax":-122.323149,"ymax":37.851575,"spatialReference":{"wkid":4326}}},"operationalLayers":[],"baseMap":{"title":"World_Terrain_Base","baseMapLayers":[{"url":"https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer"}]},"exportOptions":{"outputSize":[800,800]},"layoutOptions":{"titleText":"City Land Use Map","authorText":"Print by: XYZ","copyrightText":"Bishop-maps","scaleBarOptions":{"metricUnit":"esriKilometers","metricLabel":"km","nonMetricUnit":"esriMiles","nonMetricLabel":"mi"}}}


