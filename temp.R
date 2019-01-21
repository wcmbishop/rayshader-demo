library(rayshader)
library(rgl)
library(magick)

# for(i in 1:799) {
#   montereybay[1:2+i,] %>% 
#     sphere_shade(texture = "imhof1") %>% 
#     plot_3d(montereybay[1:2+i,],fov=0,theta=90,phi=0,
#             solid = TRUE, background="white", solidlinecolor="grey50", solidcolor = "#373026",
#             water=TRUE, waterdepth = 0,watercolor = "#88DDFF")
#   rgl::snapshot3d(paste0("montbayslice_",i,".png"))
#   rgl::rgl.close()
# }

elevation_matrix <- montereybay
frames <- 5

waterdepthvalues = min(elevation_matrix)/2 - min(elevation_matrix)/2 * cos(seq(0,2*pi,length.out = frames))
thetavalues = 90 + 45 * cos(seq(0,2*pi,length.out = frames))
ambmat = ambient_shade(elevation_matrix, zscale=30)
raymat = ray_shade(elevation_matrix, zscale=30, lambert = TRUE)

i <- 1
i <- 2

# r <- rgl::open3d()
for(i in 1:frames) {
  elevation_matrix %>%
    sphere_shade(texture = "imhof3") %>%
    add_shadow(ambmat, 0.5) %>%
    add_shadow(raymat, 0.5) %>%
    plot_3d(elevation_matrix, solid = TRUE, shadow = TRUE, water = TRUE,
            waterdepth = 0, watercolor = "imhof3", wateralpha = 0.8,
            waterlinecolor = "#ffffff", waterlinealpha = 0.5, waterlinewidth = 2,
            theta = thetavalues[i], phi = 45)
  # rgl.bringtotop()
  rgl::snapshot3d(paste0("drain",i,".png"), top = TRUE)
  # rgl::rgl.close()
  # rgl::rgl.clear("all")
  # rgl::clear3d()
  # rgl::pop3d()
}


open3d()
plot3d( cube3d(col = "green") )
M <- par3d("userMatrix")
if (!rgl.useNULL())
  play3d( 
    par3dinterp(
      time = (0:2)*0.75, 
      userMatrix = list(M, 
                        rotate3d(M, pi/2, 1, 0, 0),
                        rotate3d(M, pi/2, 0, 1, 0) ) ), 
    duration = 3 )
## Not run: 
movie3d( spin3d(), duration = 5 )




# rayshader

library(rayshader)
library(raster)


# monterey ====
montereybay
dim(montereybay)
class(montereybay)

montshadow = ray_shade(montereybay,zscale=50,lambert=FALSE)
montamb = ambient_shade(montereybay,zscale=50,)
montereybay %>% 
  sphere_shade(zscale=10,texture = "imhof1") %>% 
  add_shadow(montshadow,0.5) %>%
  add_shadow(montamb) %>%
  plot_3d(montereybay,zscale=50,fov=0,theta=-45,phi=45,windowsize=c(1000,800),zoom=0.75,
          water=TRUE, waterdepth = 0, wateralpha = 0.5,watercolor = "lightblue",
          waterlinecolor = "white",waterlinealpha = 0.3)
save_3dprint("montereybay.stl")


# san francisco ====
# overlay_img <-
local_img <- raster::raster(file.path("data", "sanfran.tiff"))
sf_map <-  png::readPNG(file.path("data", "sanfran-sat.png"))
# local_img <- raster::raster(file.path("data-raw", "n38w123", "imgn38w123_1.img"))
elmat = matrix(raster::extract(local_img, raster::extent(local_img), buffer=1000),
               nrow=ncol(local_img), ncol=nrow(local_img))

# xmax <- ncol(elmat)
# ymax <- nrow(elmat)
# xrange <- floor(c(0.47, 0.65) * xmax)
# yrange <- floor(c(0.18, 0.3) * ymax)

elmat_sub <- elmat
# elmat_sub <- elmat[xrange[1]:xrange[2], yrange[1]:yrange[2]]
# dim(elmat_sub)

ambmat = ambient_shade(elmat_sub, zscale=30)
raymat = ray_shade(elmat_sub, zscale=30, lambert = TRUE)

# 2D
elmat_sub %>%
  sphere_shade(texture = "imhof4", sunangle = 315) %>%
  add_water(detect_water(elmat_sub), color="desert") %>%
  add_overlay(sf_map, alphalayer = 0.7) %>%
  add_shadow(raymat, 0.5) %>%
  add_shadow(ambmat, 0.5) %>%
  plot_map()


# 3D
zscale <- 30  # 30 is correct
elmat_sub %>% 
  sphere_shade(zscale=zscale, texture = "imhof1") %>% 
  add_water(detect_water(elmat_sub), color="imhof1") %>%
  add_overlay(sf_map, alphalayer = 0.7) %>%
  add_shadow(raymat, 0.5) %>%
  add_shadow(ambmat, 0.5) %>%
  plot_3d(elmat_sub, zscale=zscale, fov=0, theta=-45, phi=45,
          windowsize=c(1000,800), zoom=0.75, soliddepth = -100/30,
          water=TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.3)


# REST API elevation ==== 
# https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage?bbox=-122.597807%2C37.688743%2C-122.323149%2C37.851575&bboxSR=4326&size=400%2C400&imageSR=4326&time=&format=jpgpng&pixelType=F32&noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=&mosaicRule=&renderingRule=&f=html
url <- "https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage?bbox=-122.597807%2C37.688743%2C-122.323149%2C37.851575&bboxSR=4326&size=400%2C400&imageSR=4326&time=&format=tiff&pixelType=F32&noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=&mosaicRule=&renderingRule=&f=image"

# map image
# json: {"mapOptions":{"extent":{"xmin":-122.597807,"ymin":37.688743,"xmax":-122.323149,"ymax":37.851575,"spatialReference":{"wkid":4326}}},"operationalLayers":[],"baseMap":{"title":"World_Terrain_Base","baseMapLayers":[{"url":"https://services.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer"}]},"exportOptions":{"outputSize":[400,400]},"layoutOptions":{"titleText":"City Land Use Map","authorText":"Print by: XYZ","copyrightText":"Bishop-maps","scaleBarOptions":{"metricUnit":"esriKilometers","metricLabel":"km","nonMetricUnit":"esriMiles","nonMetricLabel":"mi"}}}
# url: https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export Web Map Task/execute?f=json&Format=PNG32&Layout_Template=MAP_ONLY&Web_Map_as_JSON={"mapOptions":{"extent":{"xmin":-122.597807,"ymin":37.688743,"xmax":-122.323149,"ymax":37.851575,"spatialReference":{"wkid":4326}}},"operationalLayers":[],"baseMap":{"title":"World_Terrain_Base","baseMapLayers":[{"url":"https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer"}]},"exportOptions":{"outputSize":[800,800]},"layoutOptions":{"titleText":"City Land Use Map","authorText":"Print by: XYZ","copyrightText":"Bishop-maps","scaleBarOptions":{"metricUnit":"esriKilometers","metricLabel":"km","nonMetricUnit":"esriMiles","nonMetricLabel":"mi"}}}


