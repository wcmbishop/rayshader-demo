

#' Read an elevation image file (like a .tif) into 
#' an elevation matrix.
#'
#' @param file file path
#'
#' @return A two-dimensional matrix, where each entry in the matrix is the
#'  elevation at that point.
#'
read_elevation_file <- function(file) {
  elev_img <- raster::raster(file)
  elev_matrix <- matrix(
    raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
    nrow = ncol(elev_img), ncol = nrow(elev_img)
  )
  elev_matrix
}


