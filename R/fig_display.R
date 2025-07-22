#' Display Figure
#'
#' This function displays a ggplot object using specified dimensions and resolution settings. It saves the plot temporarily and opens it for viewing.
#'
#' @param fig A ggplot object to be displayed.
#' @param fdim A list of 3 elements: 'width', 'height', and 'dpi', each with numeric values. The width must be less than or equal to 9, the height must be less than or equal to 5, and the dpi must be greater than or equal to 600.
#'
#' @return The original ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Display a ggplot object with default dimensions
#' x <- fig_display(ggplot())
#' }
fig_display <- function(fig, fdim = list(width = 9, height = 5, dpi = 600)) {

 stopifnot(
  "`fdim` must be provided." = !is.na(fdim),
  "`fdim` must be a list." = is.list(fdim),
  "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = length(fdim) == 3,
  "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = names(fdim) == c('width', 'height', 'dpi'),
  "`fdim` values must be numeric." = is.numeric(unlist(fdim)),
  "`fdim$width` must be less or equal to 9." = fdim$width <= 9,
  "`fdim$height` must be less or equal to 5." = fdim$height <= 5,
  "`fdim$dpi` must be greater or equal to 600." = fdim$dpi >= 600
 )

 temp_jpeg <- tempfile(fileext = ".jpeg")
 ggplot2::ggsave(filename = temp_jpeg, plot = fig, width = fdim$width, height = fdim$height, dpi = fdim$dpi, units = "in", device = "jpeg", quality = 100)
 img <- magick::image_read(temp_jpeg)
 magick::image_browse(img)

 return(fig)
}
