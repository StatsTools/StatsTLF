# S4 Class 'ContentBackbone' ---------------------------------------------------
setClass(
 'ContentBackbone',
 slots = c(
  title = 'character',
  type = 'character',
  fun = 'function'
 )
)

# 'ContentBackbone' methods -----------------------------------------------------
setGeneric('create_content_method', function(x, value, subtitle, population, section, fdim, ...) standardGeneric('create_content_method'))
setMethod('create_content_method', 'ContentBackbone', function(x, value, subtitle, population, section, fdim, ...) {

 cat('  \u2500 Creating content:', x@type, '->', x@title, '...')

 content <- new(
  'Content',
  section = section,
  title = x@title,
  subtitle = subtitle,
  population = population,
  type = x@type,
  content = value,
  fdim = fdim
 )

 cat(' Done!\n')

 return(content)
})

# S4 Class 'Content' -----------------------------------------------------------

if (is.null(getClassDef("flextable"))) {
  setOldClass('flextable')
}

if (is.null(getClassDef("gg"))) {
  setOldClass("gg")
}

if (is.null(getClassDef("tbl_df"))) {
  setOldClass('tbl_df')
}

if (is.null(getClassDef("gtable"))) {
  setOldClass("gtable")
}

if (is.null(getClassDef("flextableORggORtbl_dfORgtable"))) {
  setClassUnion('flextableORggORtbl_dfORgtable', c('flextable', 'gg', 'tbl_df', 'gtable'))
}


setClass(
 'Content',
 slots = c(
  section = 'character',
  title = 'character',
  subtitle = 'character',
  population = 'character',
  type = 'character',
  content = 'flextableORggORtbl_dfORgtable',
  fdim = 'list'
 )
)

# 'Content' methods ------------------------------------------------------------

setGeneric('prepare_to_export_method', function(x, number, doc, sep_subtitle, sep_population, fig_path, last, language, supp) standardGeneric('prepare_to_export_method'))
setMethod('prepare_to_export_method', 'Content', function(x, number, doc, sep_subtitle, sep_population, fig_path, last, language, supp) {

 if (language == 'PT-BR') {
  table_text <- 'Tabela '
  figure_text <- 'Figura '
  listing_text <- 'Lista '
 } else if (language == 'EN-US') {
  table_text <- 'Table '
  figure_text <- 'Figure '
  listing_text <- 'Listing '
 }

 if (x@type == 'T') {
   if (supp == TRUE) {
     title_text <- paste0(table_text, 'S', number, ': ', x@title)
   } else {
     title_text <- paste0(table_text, number, ': ', x@title)
   }
  text_style <- 'StatsTLF T\u00edtulo 2'
  else_style <- NA_character_
 } else if (x@type == 'F') {
   if (supp == TRUE) {
     title_text <- paste0(figure_text, 'S', number, ': ', x@title)
   } else {
     title_text <- paste0(figure_text, number, ': ', x@title)
   }
  text_style <- 'StatsTLF T\u00edtulo 2'
  else_style <- 'StatsTLF Normal Centralizado'
  img_path <- paste0(fig_path, '/', 'Figure_', number)
 } else if (x@type == 'L') {
   if (supp == TRUE) {
     title_text <- paste0(listing_text, 'S', number, ': ', x@title)
   } else {
     title_text <- paste0(listing_text, number, ': ', x@title)
   }
  text_style <- 'StatsTLF T\u00edtulo 2'
  else_style <- 'StatsTLF Tabela 1'
 }

 if (!is.na(x@subtitle)) {
  if (!is.na(x@population)) {
   if (sep_subtitle == 'newline' & sep_population == 'newline') fpar_text <- officer::fpar(title_text, officer::run_linebreak(), x@subtitle, officer::run_linebreak(), x@population)
   if (sep_subtitle == 'newline' & sep_population != 'newline') fpar_text <- officer::fpar(title_text, officer::run_linebreak(), x@subtitle, sep_population, x@population)
   if (sep_subtitle != 'newline' & sep_population == 'newline') fpar_text <- officer::fpar(title_text, sep_subtitle, x@subtitle, officer::run_linebreak(), x@population)
   if (sep_subtitle != 'newline' & sep_population != 'newline') fpar_text <- officer::fpar(title_text, sep_subtitle, x@subtitle, sep_population, x@population)
  } else {
   if (sep_subtitle == 'newline') fpar_text <- officer::fpar(title_text, officer::run_linebreak(), x@subtitle)
   if (sep_subtitle != 'newline') fpar_text <- officer::fpar(title_text, sep_subtitle, x@subtitle)
  }
 } else {
  if (!is.na(x@population)) {
   if (sep_population == 'newline') fpar_text <- officer::fpar(title_text, officer::run_linebreak(), x@population)
   if (sep_population != 'newline') fpar_text <- officer::fpar(title_text, sep_population, x@population)
  } else {
   fpar_text <- officer::fpar(title_text)
  }
 }

 content_prepared <- list(
  fpar_text = fpar_text,
  text_style = text_style,
  else_style = else_style,
  content = x@content
 )

 cat('    \u250C\u2500', title_text, '\n')
 if (!is.na(x@subtitle)) cat('    \u251C\u2500\u2500 Subtitle:', x@subtitle, '\n')
 if (!is.na(x@population)) cat('    \u251C\u2500\u2500 Population:', x@population, '\n')

 if (x@type == 'F') {
  ggplot2::ggsave(paste0(img_path, '.jpeg'), x@content, width = x@fdim$width, height = x@fdim$height, dpi = x@fdim$dpi, units = "in", device = 'jpeg', quality = 100)
  ggplot2::ggsave(paste0(img_path, '.pdf'), x@content, width = x@fdim$width, height = x@fdim$height, dpi = x@fdim$dpi, units = "in", device = cairo_pdf)

  if (x@fdim$width != 9 | x@fdim$height != 5) {
    customfdim_dir <- system.file("", package = "StatsTLF")
    customfdim_name <- paste0('/custom_fdim_figure_', language, '.jpeg')
    customfdim_path <- paste0(customfdim_dir, customfdim_name)

   doc <- doc |>
    officer::body_add_fpar(content_prepared$fpar_text, style = content_prepared$text_style) |>
    officer::body_add_img(src = customfdim_path, width = 9, height = 5, style = content_prepared$else_style)
  } else {
   doc <- doc |>
    officer::body_add_fpar(content_prepared$fpar_text, style = content_prepared$text_style) |>
    officer::body_add_img(src = paste0(img_path, '.jpeg'), width = 9, height = 5, style = content_prepared$else_style)
  }
 } else if (x@type == 'T') {
  doc <- doc |>
   officer::body_add_fpar(content_prepared$fpar_text, style = content_prepared$text_style) |>
   flextable::body_add_flextable(value = content_prepared$content)
 } else if (x@type == 'L') {
  doc <- doc |>
   officer::body_add_fpar(content_prepared$fpar_text, style = content_prepared$text_style) |>
   officer::body_add_table(value = content_prepared$content, style = content_prepared$else_style)
 }

 if (last == FALSE) doc <- doc |> officer::body_add_break()

 cat('    \u2514\u2500 Done!\n')

 return(doc)
})

# S4 Class 'ContentPackage' ----------------------------------------------------

setClass(
 'ContentPackage',
 slots = c(
  content_list = 'list',
  start_number = 'list',
  sep_subtitle = 'character',
  sep_population = 'character',
  language = 'character'
 )
)

# 'ContentPackage' methods -----------------------------------------------------

setGeneric('add_to_package_method', function(x, content) standardGeneric('add_to_package_method'))
setMethod('add_to_package_method', 'ContentPackage', function(x, content) {

 cat('      Adding content to package ...')

 x@content_list <- append(x@content_list, list(content))

 cat(' Done!\n')

 return(x)
})

setGeneric('export_package_method', function(x, report_name, template_name, supp = FALSE, dataset = FALSE) standardGeneric('export_package_method'))
setMethod('export_package_method', 'ContentPackage', function(x, report_name, template_name, supp = FALSE, dataset = FALSE) {

 cat('  Exporting content package:\n')

 if (dataset) {
   zipfolder <- here::here('04_Datasets')

   unlink(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")), recursive = TRUE, force = TRUE)
   dir.create(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")))

   saveRDS(x@content_list, paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), ".RDS"))
   Sys.chmod(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), ".RDS"), mode = "0444")

   cat('  Done!\n')

   cat(paste0('\n\n Datasets avaliable in: ', zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")))

 } else {
   zipfolder <- here::here('05_Results')

   output <- list(
     dir = tempfile(pattern = format(Sys.time(), "%Y_%m_%d_%H_%M_%S_"))
   )
   dir.create(output$dir)

   types <- sapply(x@content_list, function(x) return(x@type))
   sections <- sapply(x@content_list, function(x) return(x@section))
   sections_levels <- unique(sections)

   content_numbers <- tibble::tibble(ID = seq(1, length(x@content_list)), type = factor(types, levels = c('T', 'F', 'L')), section = factor(sections, levels = sections_levels)) |>
     dplyr::group_by(type) |>
     dplyr::group_split() |>
     lapply(function(type_data) {
       if (unique(type_data$type) == 'T') start <- x@start_number$T
       else if (unique(type_data$type) == 'F') start <- x@start_number$F
       else if (unique(type_data$type) == 'L') start <- x@start_number$L

       type_data |>
         dplyr::mutate(number = seq(start, start + dplyr::n() - 1))
     }) |>
     purrr::reduce(dplyr::bind_rows) |>
     dplyr::mutate(last = ID == dplyr::n()) |>
     dplyr::arrange(ID)

   sections_start <- content_numbers |>
     dplyr::group_by(section) |>
     dplyr::summarise(section_start = min(ID))

   template_path <- paste0(here::here('00_Template'), '\\', template_name)

   doc <- officer::read_docx(path = template_path) |>
     officer::body_add_toc() |>
     officer::body_add_break()
   for (i in seq(1, length(x@content_list))) {
     if (i %in% c(sections_start$section_start)) {
       if (!is.na(sections_start |> dplyr::filter(section_start == i) |> dplyr::pull(section))) doc <- officer::body_add_fpar(doc, officer::fpar(sections_start |> dplyr::filter(section_start == i) |> dplyr::pull(section)), style = 'StatsTLF T\u00edtulo 1')
       doc <- prepare_to_export_method(x@content_list[[i]], content_numbers$number[i], doc, x@sep_subtitle, x@sep_population, output$dir, last = content_numbers$last[i], language = x@language, supp = supp)
     } else {
       doc <- prepare_to_export_method(x@content_list[[i]], content_numbers$number[i], doc, x@sep_subtitle, x@sep_population, output$dir, last = content_numbers$last[i], language = x@language, supp = supp)
     }
   }
   print(doc, target = paste0(output$dir, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.docx'))

   unlink(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")), recursive = TRUE, force = TRUE)
   dir.create(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")))

   files_to_copy <- list.files(output$dir, full.names = TRUE)
   file.copy(from = files_to_copy, to = paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
   Sys.chmod(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.docx'), mode = "0444", use_umask = FALSE)

   cat('  Done!\n')

   cat(paste0('\n\n Report avaliable in: ', zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")))

 }

 return(invisible(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"))))
})
