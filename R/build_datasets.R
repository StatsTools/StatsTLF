#' Build Datasets Package for Submission
#'
#' This function generates a comprehensive package with log file for a specified dataset package and the datasets associated. The log includes session information, backbone file details, and caller content logs. The package name must be provided as a character string, and the corresponding directory must exist in '03_Algorithm'.
#'
#' @param pkg_name A character string specifying the name of the package.
#'
#' @return The path to the generated folder.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Build a log file for the package 'Teste'
#' x <- build_datasets('Teste')
#' }
build_datasets <- function(pkg_name) {

  # Validation Step -------------------------------------------------------------
  stopifnot(
    "`pkg_name` must be a character." = is.character(pkg_name),
    "`pkg_name` cannot be an array." = length(pkg_name) == 1
  )

  path <- here::here('03_Algorithm')
  path <- paste0(path, '\\', pkg_name)

  stopifnot("Package name folder doesn't exist in `03_Algorithm`." = dir.exists(path))
  # -----------------------------------------------------------------------------

  zipfolder <- here::here('04_Datasets')

  output <- list(
    dir = tempfile(pattern = format(Sys.time(), "%Y_%m_%d_%H_%M_%S_"))
  )
  dir.create(output$dir)

  unlink(paste0(zipfolder, '/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d")), recursive = TRUE, force = TRUE)

  backbone_names <- sub("\\.R$", "", list.files(paste0(path, '\\backbones')))
  backbone_files <- list.files(paste0(path, '\\backbones'), full.names = TRUE)

  mapply(function(file, name) {
    logrx::axecute(file, paste0(name, '.log'), output$dir)
  }, file = backbone_files, name = backbone_names, SIMPLIFY = FALSE)

  backbone_logs <- lapply(backbone_names, function(name) {
    data <- head(readLines(paste0(output$dir, "\\", name, ".log")), -6)
    begin1 <- which(data == '-                          User and File Information                           -')
    begin2 <- which(data == '-                             Session Information                              -')
    begin3 <- which(data == '-                          Used Package and Functions                          -')

    data <- c(data[(begin1 - 1):(begin2 - 2)], data[(begin3 - 1):(length(data))])

    return(data)
  })

  logrx::axecute(paste0(path, '\\caller_contents.R'), 'caller_contents.log', output$dir)

  caller_contents_log <- head(readLines(paste0(output$dir, '\\caller_contents.log')), -6)
  begin1 <- which(caller_contents_log == '-                          User and File Information                           -')
  begin2 <- which(caller_contents_log == '-                             Session Information                              -')
  begin3 <- which(caller_contents_log == '-                          Used Package and Functions                          -')

  caller_contents_log <- c(caller_contents_log[(begin1 - 1):(begin2 - 2)], caller_contents_log[(begin3 - 1):(length(caller_contents_log))])

  logrx::axecute(paste0(path, '\\caller_', pkg_name, '.R'), paste0('caller_', pkg_name, '.log'), output$dir)

  caller_pkg_log <- head(readLines(paste0(output$dir, '\\caller_', pkg_name, '.log')), -6)

  begin1 <- which(caller_pkg_log == '-                             Session Information                              -')
  begin2 <- which(caller_pkg_log == '-                          Used Package and Functions                          -')

  init_log <- c(caller_pkg_log[1:7], caller_pkg_log[(begin1 - 1):(begin2 - 1)])
  caller_pkg_log <- c(caller_pkg_log[8:14], caller_pkg_log[(begin2 - 1):(length(caller_pkg_log))])


  log_file <- file.path(output$dir, paste0('SAR Log File - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.log'))
  file.create(log_file)
  write('────────────────────────────────────────────────────────────────────────────────', file = log_file, append = TRUE)
  write('-                              Session Information                             -', file = log_file, append = TRUE)
  write('────────────────────────────────────────────────────────────────────────────────', file = log_file, append = TRUE)
  write(init_log, file = log_file, append = TRUE)
  write('────────────────────────────────────────────────────────────────────────────────', file = log_file, append = TRUE)
  write('-                             Backbones Information                            -', file = log_file, append = TRUE)
  write('────────────────────────────────────────────────────────────────────────────────', file = log_file, append = TRUE)
  lapply(backbone_logs, function(log)  {
    write(log, file = log_file, append = TRUE)
    write('', file = log_file, append = TRUE)
  })
  write('────────────────────────────────────────────────────────────────────────────────', file = log_file, append = TRUE)
  write('-                          Caller Contents Information                         -', file = log_file, append = TRUE)
  write('────────────────────────────────────────────────────────────────────────────────', file = log_file, append = TRUE)
  write(caller_contents_log, file = log_file, append = TRUE)
  write('────────────────────────────────────────────────────────────────────────────────', file = log_file, append = TRUE)
  write('-                            Main Caller Information                           -', file = log_file, append = TRUE)
  write('────────────────────────────────────────────────────────────────────────────────', file = log_file, append = TRUE)
  write('', file = log_file, append = TRUE)
  write(caller_pkg_log, file = log_file, append = TRUE)
  write('', file = log_file, append = TRUE)
  write('--------------------------------------------------------------------------------', file = log_file, append = TRUE)
  write('-                               Log Output File                                -', file = log_file, append = TRUE)
  write('--------------------------------------------------------------------------------', file = log_file, append = TRUE)

  stopifnot('Error generating log. Please check package files.' = file.exists(paste0(here::here('04_Datasets'), '/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"))))

  write(paste0('Log File Name: Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.log'), file = log_file, append = TRUE)
  write(paste0('Log Folder: ', here::here('04_Datasets'), '/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets'), file = log_file, append = TRUE)

  rds_files_simp <- list.files(paste0(here::here('04_Datasets'), '/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets'), pattern = "\\.(RDS|xpt)$", full.names = FALSE)
  rds_files <- list.files(paste0(here::here('04_Datasets'), '/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets'), pattern = "\\.(RDS|xpt)$", full.names = TRUE)

  write(paste0('Datasets File HashSum:'), file = log_file, append = TRUE)

  for (i in seq_along(rds_files)) {
    write(paste0('      ', rds_files_simp[i], ': ', digest::sha1(readLines(rds_files[i], warn = FALSE))), file = log_file, append = TRUE)
  }

  file.copy(paste0(output$dir, '\\SAR Log File - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.log'),
            paste0(here::here('04_Datasets'), '/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.log'),
            overwrite = TRUE)
  Sys.chmod(paste0(here::here('04_Datasets'), '/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.log'), mode = "0444")

  cat('\n Log file HashSum (Save this hash in a safe place!): ', digest::sha1(readLines(paste0(here::here('04_Datasets'), '/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.log'), warn = FALSE)))

  return(invisible(paste0(here::here('04_Datasets'), '/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets/Datasets - ', pkg_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.log')))
}
