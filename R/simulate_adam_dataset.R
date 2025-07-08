#' Simulate an analysis dataset based on created dataset
#'
#' @param dataset A tibble created via create_adam_dataset function.
#' @param n A number with the number of rows to be simulated
#'
#' @return A tibble with the data simulation.
#' @export
#'
#' @examples
#' x <- simulate_adam_dataset(create_adam_dataset('ADSL.xlsx'))
simulate_adam_dataset <- function(dataset, n = 10) {

 # Validation Step -------------------------------------------------------------
  stopifnot(
    "`n` must be provided." = !is.na(n),
    "`n` must be a character." = is.numeric(n),
    "`n` cannot be an array." = length(n) == 1
  )
 # -----------------------------------------------------------------------------

  stopifnot("Validation error: The dataset does not conform to the defined metadata." = StatsTLF::validate_adam_dataset(dataset))

  path <- attr(dataset, "path")

  cols <- lapply(names(dataset), function(colname) {
    col <- dataset[[colname]]

    group <- attr(col, "group")
    name <- attr(col, "name")
    label <- attr(col, "label")
    dtype <- attr(col, "dtype")
    type <- attr(col, "type")
    source <- attr(col, "source")
    related_vars <- attr(col, "related_vars")
    closed <- attr(col, "closed")
    factor_levels <- attr(col, "factor_levels")
    factor_labels <- attr(col, "factor_labels")
    rule <- attr(col, "rule")
    use_levels <- attr(col, "use_levels")

    is_simulated <- type %in% c("Fixed", "External")

    simulated_dataset <- if (!is_simulated) {
      if (is.factor(col)) {
        factor(rep(NA, n), levels = levels(col))
      } else if (inherits(col, "Date")) {
        rep(as.Date(NA), n)
      } else if (is.numeric(col)) {
        rep(NA_real_, n)
      } else if (is.character(col)) {
        rep(NA_character_, n)
      } else {
        stop(paste("Data Type not supported:", class(col)))
      }
    } else {
      if (is.factor(col)) {
        factor(sample(levels(col), n, replace = TRUE),
               levels = levels(col))
      } else if (inherits(col, "Date")) {
        as.Date("2000-01-01") + sample(0:10000, n, replace = TRUE)
      } else if (is.numeric(col)) {
        rnorm(n, mean = 50, sd = 10)
      } else if (is.character(col)) {
       toupper(replicate(n, paste0(sample(letters, 10, replace = TRUE), collapse = "")))
      } else {
        stop(paste("Data Type not supported:", class(col)))
      }
    }

    attr(simulated_dataset, "group") <- group
    attr(simulated_dataset, "name") <- name
    attr(simulated_dataset, "label") <- label
    attr(simulated_dataset, "dtype") <- dtype
    attr(simulated_dataset, "type")  <- type
    attr(simulated_dataset, "source")  <- source
    attr(simulated_dataset, "related_vars")  <- related_vars
    attr(simulated_dataset, "closed") <- closed
    attr(simulated_dataset, "factor_levels") <- factor_levels
    attr(simulated_dataset, "factor_labels") <- factor_labels
    attr(simulated_dataset, "rule") <- rule
    attr(simulated_dataset, "use_levels") <- use_levels

    return(simulated_dataset)
  })

  simulated_dataset <- tibble::tibble(!!!rlang::set_names(cols, names(dataset)))

  attr(simulated_dataset, "path") <- path

  return(simulated_dataset)
}

