#' Read, process and feature extraction of E4 data
#' @description Reads the raw ZIP file using `read_e4`, performs analyses with `ibi_analysis` and `eda_analysis`. 
#' @return An object with processed data and analyses, object of class 'e4_analysis'.
#' @export
read_and_process_e4 <- function(zipfile, tz = Sys.timezone()){
  
  
  data <- read_e4(zipfile, tz)
  
  ibi <- ibi_analysis(data$IBI)
  
  eda <- eda_analysis(data)
  
  
  
list(
  data = data,
  ibi = ibi,
  eda = eda
)  
}
