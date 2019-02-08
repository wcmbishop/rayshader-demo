
#' Build a gif of 3D rayshader plots
#'
#' @param hillshade Hillshade/image to be added to 3D surface map.
#' @param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' @param file file path for .gif
#' @param duration gif duration in seconds (framerate will be duration/n_frames)
#' @param ... additional arguments passed to rayshader::plot_3d(). See Details for more info.
#'
#' @details This function is designed to be a pipe-in replacement for rayshader::plot_3d(),
#' but it will generate a 3D animated gif. Any inputs with lengths >1 will 
#' be interpreted as "animation" variables, which will be used to generate 
#' individual animation frames -- e.g. a vector of theta values would produce
#' a rotating gif. Inputs to plot_3d() that are meant to have length >1 
#' (specifically "windowsize") will be excluded from this process.
#'
#' @return file path of .gif file created
#' 
#' @examples
#' # MONTEREREY BAY WATER DRAINING
#' # ------------------------------
#' # define transition variables
#' n_frames <- 180
#' waterdepths <- transition_values(from = 0, to = min(montereybay), steps = n_frames) 
#' thetas <- transition_values(from = -45, to = -135, steps = n_frames)
#' # generate gif
#' zscale <- 50
#' montereybay %>% 
#'   sphere_shade(texture = "imhof1", zscale = zscale) %>%
#'   add_shadow(ambient_shade(montereybay, zscale = zscale), 0.5) %>%
#'   add_shadow(ray_shade(montereybay, zscale = zscale, lambert = TRUE), 0.5) %>%
#'   save_3d_gif(montereybay, file = "montereybay.gif", duration = 6,
#'               solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
#'               watercolor = "imhof3", wateralpha = 0.8, 
#'               waterlinecolor = "#ffffff", waterlinealpha = 0.5,
#'               waterdepth = waterdepths/zscale, 
#'               theta = thetas, phi = 45)
#' 
save_3d_gif <- function(hillshade, heightmap, file, duration = 5, ...) {
  require(rayshader)
  require(magick)
  require(rgl)
  require(gifski)
  require(rlang)
  
  # capture dot arguments and extract variables with length > 1 for gif frames
  dots <- rlang::list2(...)
  var_exception_list <- c("windowsize")
  dot_var_lengths <- purrr::map_int(dots, length)
  gif_var_names <- names(dots)[dot_var_lengths > 1 & 
                                 !(names(dots) %in% var_exception_list)]
  # split off dot variables to use on gif frames
  gif_dots <- dots[gif_var_names]
  static_dots <- dots[!(names(dots) %in% gif_var_names)]
  gif_var_lengths <- purrr::map_int(gif_dots, length)
  # build expressions for gif variables that include index 'i' (to use in the for loop)
  gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
  gif_exprs <- exprs(!!!gif_expr_list)
  names(gif_exprs) <- names(gif_dots)
  message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
  
  # TODO - can we recycle short vectors?
  if (length(unique(gif_var_lengths)) > 1) 
    stop("all gif input vectors must be the same length")
  n_frames <- unique(gif_var_lengths)
  
  # generate temp .png images
  temp_dir <- tempdir()
  img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
  on.exit(unlink(img_frames))
  message(paste("Generating", n_frames, "temporary .png images..."))
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    rgl::clear3d()
    hillshade %>%
      plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
    rgl::snapshot3d(img_frames[i])
  }
  
  # build gif
  message("Generating .gif...")
  magick::image_write_gif(magick::image_read(img_frames), 
                          path = file, delay = duration/n_frames)
  message("Done!")
  invisible(file)
}


plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}


#' Create a numeric vector of transition values.
#' @description This function helps generate a sequence 
#' of numeric values to transition "from" a start point
#' "to" some end point. The transition can be "one_way" 
#' (meaning it ends at the "to" point) or "two_way" (meaning
#' we return back to end at the "from" point).
#'
#' @param from starting point for transition values
#' @param to ending point (for one-way transitions) or turn-around point 
#'           (for two-way transitions)
#' @param steps the number of steps to take in the transation (i.e. the length
#'              of the returned vector)
#' @param one_way logical value to determine if we should stop at the "to" value
#'                (TRUE) or turn around and return to the "from" value (FALSE)
#' @param type string defining the transition type - currently suppoerts "cos"
#'             (for a cosine curve) and "lin" (for linear steps)
#'
#' @return a numeric vector of transition values
#' 
transition_values <- function(from, to, steps = 10, 
                              one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps/2)), 
                seq(-1, 1, length.out = ceiling(steps/2)))
    }
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y 
  }
  
  middle - half_width * scaling
}


