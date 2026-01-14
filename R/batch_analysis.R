#' Batch analysis
#'
#' Read and process all ZIP files in a directory
#'
#' @param path_in input path
#' @param path_out output path
#' @export
batch_analysis <- function(path_in = NULL, path_out = ".") {
  
  if (is.null(path_in)) {
    if (.Platform$OS.type == "windows") {
      path_in <- utils::choose.dir()
    } else {
      stop("Provide an input directory (argument path_in)")
    }
  }
  
  zips <- list.files(path_in, pattern = "[.]zip$", recursive = TRUE, full.names = TRUE)
  
  for (i in seq_along(zips)) {
    .log_info("----- %s -----", zips[i])
    out <- read_and_process_e4(zips[i])
    
    fn_root <- basename(tools::file_path_sans_ext(zips[i]))
    out_file <- file.path(path_out, paste0(fn_root, ".rds"))
    
    saveRDS(out, out_file)
    
    .log_info("----- %s/%s complete -----", i, length(zips))
  }
}
