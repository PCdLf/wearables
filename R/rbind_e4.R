#' Row-bind E4 datasets
#' @param data An object read in by read_e4
#' @importFrom dplyr bind_rows
#' @export
rbind_e4 <- function(data){
  
  out <- list()
  
  nms <- names(data[[1]])
  
  for(name in nms){
    
    # retrieve data
    dat <- lapply(data, "[[", name)
    out[[name]] <- bind_rows(dat)
    
  }
  
  out
}
