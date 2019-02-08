# download-demo-file.R
# download demo elevation .tif from rayshader

loadzip = file.path("data", "dem_01.tif.zip") 
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
unzip(loadzip, exdir = "data", files = "dem_01.tif")
unlink(loadzip)
