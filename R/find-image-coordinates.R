

#' Translate the given long/lat coordinates into an image position (x, y).
#'
#' @param long longitude value
#' @param lat latitude value
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param image_width image width, in pixels
#' @param image_height image height, in pixels
#'
#' @return named list with elements "x" and "y" defining an image position
#'
find_image_coordinates <- function(long, lat, bbox, image_width, image_height) {
  x_img <- round(image_width * (long - min(bbox$p1$long, bbox$p2$long)) / abs(bbox$p1$long - bbox$p2$long))
  y_img <- round(image_height * (lat - min(bbox$p1$lat, bbox$p2$lat)) / abs(bbox$p1$lat - bbox$p2$lat))
  list(x = x_img, y = y_img)
}


