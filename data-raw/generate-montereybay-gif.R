# generate montereybay.gif
library(httr)
library(glue)
library(jsonlite)
library(raster)
library(rayshader)
source(file.path("R", "rayshader-gif.R"))

# montery water gif - easier ====
n_frames <- 180
zscale <- 50
# calculate input vectors for gif frames
waterdepths <- transition_values(start = 0, end = min(montereybay), steps = n_frames) 
thetas <- transition_values(start = -45, end = -135, steps = n_frames)
# generate gif
montereybay %>% 
  sphere_shade(texture = "imhof1", zscale = 10) %>%
  add_shadow(ambient_shade(montereybay, zscale = zscale), 0.5) %>%
  add_shadow(ray_shade(montereybay, zscale = zscale, lambert = TRUE), 0.5) %>%
  create_3d_gif(montereybay, file = file.path("images", "montereybay.gif"), duration = 6,
                solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
                watercolor = "imhof3", wateralpha = 0.8, 
                waterlinecolor = "#ffffff", waterlinealpha = 0.5,
                waterdepth = waterdepths/zscale, 
                theta = thetas, phi = 45)
