#' Read and process all ZIP files in a directory
#' @export
#' @importFrom futile.logger flog.info
#' @importFrom utils choose.dir
batch_analysis <- function(path_in = choose.dir(), path_out = "."){
  
  
  # path <- "C:\\repos2\\e4dashboard\\BVI"
  zips <- list.files(path_in, pattern = "[.]zip$", recursive = TRUE, full.names = TRUE)
  
  for(i in seq_along(zips)){
  
    flog.info(paste("----- ", zips[i], " -----"))
    out <- read_and_process_e4(zips[i])
    
    fn_root <- basename(tools::file_path_sans_ext(zips[i]))
    out_file <- file.path(path_out, paste0(fn_root, ".rds"))
    
    saveRDS(out, out_file)
    
    flog.info(paste("----- ", i, "/", length(zips), " complete -----"))
    
  }
  
}
