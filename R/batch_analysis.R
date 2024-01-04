#' Batch analysis
#'
#' Read and process all ZIP files in a directory
#'
#' @param path_in input path
#' @param path_out output path
#' @export
#' @importFrom futile.logger flog.info
batch_analysis <- function(path_in = NULL, path_out = ".") {
  if (is.null(path_in) && .Platform$OS.type == "windows") {
    path_in <- utils::choose.dir()
  } else {
    stop("Provide an input directory (argument path_in)")
  }

  # path <- "C:\\repos2\\e4dashboard\\BVI"
  zips <- list.files(path_in, pattern = "[.]zip$", recursive = TRUE, full.names = TRUE)

  for (i in seq_along(zips)) {
    flog.info(paste("----- ", zips[i], " -----"))
    out <- read_and_process_e4(zips[i])

    fn_root <- basename(tools::file_path_sans_ext(zips[i]))
    out_file <- file.path(path_out, paste0(fn_root, ".rds"))

    saveRDS(out, out_file)

    flog.info(paste("----- ", i, "/", length(zips), " complete -----"))
  }
}
