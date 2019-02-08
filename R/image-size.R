

#' Define image size variables from the given bounding box coordinates.
#'
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param major_dim major image dimension, in pixels. 
#'                  Default is 400 (meaning larger dimension will be 400 pixels)
#'
#' @return list with items "width", "height", and "size" (string of format "<width>,<height>")
#'
#' @examples
#' bbox <- list(
#'   p1 = list(long = -122.522, lat = 37.707),
#'   p2 = list(long = -122.354, lat = 37.84)
#' )
#' image_size <- define_image_size(bbox, 600)
#' 
define_image_size <- function(bbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}

