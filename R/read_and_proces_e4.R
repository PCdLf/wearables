#' Read, process and feature extraction of E4 data
#' @description Reads the raw ZIP file using `read_e4`, performs analyses with `ibi_analysis` and `eda_analysis`. 
#' @return An object with processed data and analyses, object of class 'e4_analysis'.
#' @rdname read_and_process_e4
#' @export
read_and_process_e4 <- function(zipfile, tz = Sys.timezone()){
  
  
  data <- read_e4(zipfile, tz)
  
  ibi <- ibi_analysis(data$IBI)
  
  #eda <- eda_analysis(data)
  
  
structure(list(
  data = data,
  ibi = ibi,
  eda = NULL
), 
class = "e4_analysis")
}


#' @rdname read_and_process_e4
#' @export
write_processed_e4 <- function(obj, out_path = "."){
  
  stopifnot(inherits(obj, "e4_analysis"))
  
  # read_e4 stored the zipname as an attribute
  zipname <- basename(attr(obj$data, "zipfile"))
  
  # Output goes to a folder with the zipfile name
  out_folder <- file.path(out_path, zipname)
  dir.create(out_folder)
  
  browser()
  #write.csv(obj$data$ , file.path(out_folder, ...)
  
  file_out <- function(data, name){
    write.csv2(data, file.path(out_folder,name), row.names = FALSE)
  }
  
  file_out(obj$data$EDA, "EDA.csv")
  file_out(obj$data$ACC, "ACC.csv")
  file_out(obj$data$TEMP, "TEMP.csv")
  file_out(obj$data$HR, "HR.csv")
  file_out(obj$data$BVP, "BVP.csv")
  file_out(obj$data$IBI, "IBI.csv")
  
  ibi_a <- data.frame(
    Time = obj$ibi$freq_analysis$Time,
    HRV = obj$ibi$freq_analysis$HRV,
    ULF = obj$ibi$freq_analysis$ULF,
    VLF = obj$ibi$freq_analysis$VLF,
    LF = obj$ibi$freq_analysis$LF,
    HF = obj$ibi$freq_analysis$HF,
    LFHF = obj$ibi$freq_analysis$LFHF
  )
  file_out(ibi_a, "IBI_FreqAnalysis.csv")
  
  # Vector with summary variables
  ibi_s <- c(obj$ibi$time_analysis, obj$ibi$summary$frequency, obj$ibi$summary$beats)
  file_out(ibi_a, "IBI_SummaryPars.csv")
  
}




