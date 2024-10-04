#' Display figure
#'
#' @param fig  A ggplot object.
#' @param fdim A list of 3 elements 'width', 'height' and 'dpi', respectively, with 3 numeric values associated. Only used for type = 'F'.
#'
#' @return A ggplot.
#' @export
#'
#' @examples
#' x <- fig_display(ggplot())
fig_display <- function(fig, fdim = list(width = 9, height = 5, dpi = 600)) {

 stopifnot(
  "`fdim` must be provided." = !is.na(fdim),
  "`fdim` must be a list." = is.list(fdim),
  "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = length(fdim) == 3,
  "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = names(fdim) == c('width', 'height', 'dpi'),
  "`fdim` values must be numeric." = is.numeric(unlist(fdim))
 )

 temp_jpeg <- tempfile(fileext = ".jpeg")
 ggplot2::ggsave(filename = temp_jpeg, plot = fig, width = fdim$width, height = fdim$height, dpi = fdim$dpi, units = "in", device = "jpeg", quality = 100)
 img <- magick::image_read(temp_jpeg)
 magick::image_browse(img)

 return(fig)
}
