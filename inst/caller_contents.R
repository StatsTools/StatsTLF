caller_contents <- function(package) {
  ## Use this to debug contents and get backbones names
  backbones <- source_backbones()

  ## Write your input contents here. Add `@content` after the `create_content()` to get the results.


  contents <- list(
    ## Write your output contents inside this list. Add ',' after each content

  )

  package <- purrr::reduce(contents, StatsTLF::add_to_package, .init = package)

  return(package)
}
