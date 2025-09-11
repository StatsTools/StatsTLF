#' Mask numbers in a string based on a pattern
#'
#' @description
#' Replaces numeric values in a string with a mock representation ("x")
#' according to a user-specified pattern. Non-numeric parts can be kept.
#'
#' @param x A character vector with the strings to be masked.
#' @param pattern A character vector specifying the pattern(s) for each string.
#'   Patterns can include:
#'   \itemize{
#'     \item{"keep"}{ - keep the value as is}
#'     \item{"i(n)"}{ - integer with n digits}
#'     \item{"f(n,m)"}{ - float with n integer digits and m decimal digits, default decimal separator '.'}
#'     \item{"f(n,m,sep)"}{ - float with n integer digits, m decimal digits, and explicit decimal separator ('.' or ',')}
#'   }
#'
#' @return A character vector with the numeric values masked according to the pattern.
#' @examples
#' mock_str("A H5N8 7,1 mas ok (N = 1000.0)", "keep+keep+i(1)+f(3,1)")
#' @export
mock_str <- function(x, pattern) {
  validate <- TRUE
  if (length(pattern) == 1) pattern <- rep(pattern, length(x))
  stopifnot(length(pattern) == length(x))

  parse_pattern <- function(p) {
    parts <- strsplit(p, "\\+")[[1]]
    lapply(parts, function(el) {
      el <- gsub(" ", "", el)
      if (el == "keep") {
        list(type = "keep")
      } else if (grepl("^i\\((\\d+)\\)$", el)) {
        list(type = "integer", int_digits = as.numeric(sub(".*\\((\\d+)\\)", "\\1", el)))
      } else if (grepl("^f\\((\\d+),(\\d+)\\)$", el)) {
        # Float com separador default (.)
        m <- regmatches(el, regexec("f\\((\\d+),(\\d+)\\)", el))[[1]]
        list(
          type = "float",
          int_digits = as.numeric(m[2]),
          dec_digits = as.numeric(m[3]),
          sep = "."
        )
      } else if (grepl("^f\\((\\d+),(\\d+),([\\.,])\\)$", el)) {
        # Float com separador explícito
        m <- regmatches(el, regexec("f\\((\\d+),(\\d+),([\\.,])\\)", el))[[1]]
        list(
          type = "float",
          int_digits = as.numeric(m[2]),
          dec_digits = as.numeric(m[3]),
          sep = m[4]
        )
      } else {
        stop("Invalid pattern element: ", el)
      }
    })
  }

  classify_number <- function(s) {
    if (grepl("^[0-9]+$", s)) {
      return(list(type = "integer"))
    }
    if (grepl("^[0-9]+([\\.,])[0-9]+$", s)) {
      sep <- regmatches(s, regexpr("[\\.,]", s))
      return(list(type = "float", sep = sep))
    }
    if (grepl("^[0-9]+[\\.,]([^0-9]|$)", s)) {
      return(list(type = "integer"))
    }
    return(list(type = "other"))
  }

  make_x <- function(p) {
    if (p$type == "keep") return(NULL)
    int_x <- paste(rep("x", p$int_digits), collapse = "")
    if (p$type == "integer") return(int_x)
    if (p$type == "float") {
      dec_x <- paste(rep("x", p$dec_digits), collapse = "")
      return(paste0(int_x, p$sep, dec_x))
    }
    stop("Unknown type")
  }

  mapply(function(string, pat) {
    if (is.na(string)) return(NA_character_)

    parsed <- parse_pattern(pat)
    numbers <- unlist(stringr::str_extract_all(string, "\\d+[\\.,]?\\d*"))

    if (validate && length(numbers) != length(parsed)) {
      stop("Number of values in the string does not match the pattern: ", string)
    }

    for (i in seq_along(parsed)) {
      if (parsed[[i]]$type == "keep") next
      info <- classify_number(numbers[i])

      if (validate) {
        if (info$type != parsed[[i]]$type) {
          stop(sprintf("Number type does not match in string '%s': expected '%s', found '%s'",
                       string, parsed[[i]]$type, info$type))
        }
        if (info$type == "float" && parsed[[i]]$sep != info$sep) {
          stop(sprintf("Decimal separator does not match in string '%s': expected '%s', found '%s'",
                       string, parsed[[i]]$sep, info$sep))
        }
      }
    }

    modified <- string
    for (i in seq_along(parsed)) {
      p <- parsed[[i]]
      if (p$type == "keep") next

      target <- numbers[i]
      pattern_match <- regexpr(target, modified, fixed = TRUE)
      if (pattern_match > 0) {
        modified <- paste0(
          substr(modified, 1, pattern_match - 1),
          make_x(p),
          substr(modified, pattern_match + nchar(target), nchar(modified))
        )
      }
    }

    modified
  }, x, pattern, SIMPLIFY = TRUE, USE.NAMES = FALSE)
}
