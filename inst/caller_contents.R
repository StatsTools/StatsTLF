caller_contents <- function(package, backbones) {
  ## USE THIS TO DEBUG CONTENTS


  ## USE THIS AS TEMPLATE FOR DATASETS BACKBONES
  # dataset <- StatsTLF::create_content(
  #   content_backbone = backbones,
  #   dataset = tibble::tibble()
  # )@content

  contents <- list(
    ## USE THIS AS TEMPLATE FOR CONTENTS BACKBONES. ADD ',' AFTER EACH CONTENT.
    # StatsTLF::create_content(
    #   content_backbone = backbones,
    #   dataset = ,
    #   subtitle = NA_character_,
    #   population = NA_character_,
    #   section = NA_character_,
    #   fdim = list(width = 9, height = 5, dpi = 600)
    # )
  )

  package <- purrr::reduce(contents, add_to_package, .init = package)

  return(package)
}
