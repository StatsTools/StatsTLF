caller_contents <- function(package) {
  ## Use this to debug contents
  backbones <- source_backbones()

  ## Use this as template for input dataset backbones
  # dataset <- StatsTLF::create_content(
  #   id = '',
  #   content_backbone = backbones,
  #   dataset = tibble::tibble()
  # )@content

  contents <- list(
    ## Use this as template for output contents backbones. Add ',' after each content
    # StatsTLF::create_content(
    #   id = '',
    #   content_backbone = backbones,
    #   dataset = tibble::tibble(),
    #   subtitle = NA_character_,
    #   population = NA_character_,
    #   section = NA_character_,
    #   fdim = list(width = 9, height = 5, dpi = 600)
    # )
  )

  package <- purrr::reduce(contents, StatsTLF::add_to_package, .init = package)

  return(package)
}
