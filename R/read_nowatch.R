#' Read Nowatch data
#' @description Reads in Nowatch data as a list, and prepends timecolumns
#' @details This function reads in a zipfile as exported by Nowatch. Then it extracts the zipfiles in a temporary folder
#' and unzips them in the same temporary folder.
#'
#' The unzipped files are csv files.
#'
#' The function returns an object of class "nowatch_data" with a prepended datetime columns.
#' The object contains a list with dataframes from the physiological signals.
#'
#' @param zipfile A zip file as exported by the instrument. Only aggregated data supported.
#' @param type The type of data contained in the zip file.
#' @param tz The timezone used by the instrument (defaults to user timezone).
#' @examples
#' \dontrun{
#'  library(wearables)
#'  read_nowatch("yourpathtohezipfile.zip")
#' }
#' @export
#' @import cli
read_nowatch <- function(zipfile,
                         tz = Sys.timezone()) {
  
  # Check if file exists
  if (!file.exists(zipfile)) {
    cli_abort("File does not exist")
  }
  
  csv_files <- unzip_files(zipfile, "csv")
  
  # Get the content before .csv and after the last _ (but include -)
  dataset_names <- gsub(".*?([A-Za-z0-9\\-]+)[.]csv", "\\1", csv_files)
  dataset_names <- toupper(dataset_names)
  dataset_names <- gsub("ACTIVITYTYPE", "ACT", dataset_names)
  dataset_names <- gsub("CADENCE", "CAD", dataset_names)
  dataset_names <- gsub("TEMPERATURE", "TEMP", dataset_names)
  dataset_names <- gsub("CORTISOLLEVELS", "CORTL", dataset_names)
  dataset_names <- gsub("HEARTBEATS", "HBR", dataset_names)
  dataset_names <- gsub("HEARTRATE", "HR", dataset_names)
  dataset_names <- gsub("RESPIRATIONRATE", "RR", dataset_names)
  dataset_names <- gsub("SLEEPSESSION", "SLEEP", dataset_names)
  dataset_names <- gsub("STRESSLEVEL", "STRESS", dataset_names)
  csv_files <- setNames(csv_files, dataset_names)
  
  csv_list <- list()
  
  for (i in 1:length(csv_files)) {
    
    file <- csv_files[i]
    
    this_file <- read.csv(file, stringsAsFactors = FALSE)
    
    rename_cols <- list(c("timestamp", "unix_timestamp"),
                        c("value", names(file)))
    
    for (j in rename_cols) {
      if (j[[1]] %in% colnames(this_file)) {
        names(this_file)[names(this_file) == j[[1]]] <- j[[2]]
      }
    }
    
    # convert unix timestamp to as.POSIXct
    if ("unix_timestamp" %in% colnames(this_file)) {
      this_file$DateTime <- as.POSIXct(this_file$unix_timestamp, origin = "1970-01-01", tz = tz)
    }
    
    csv_list[[names(file)]] <- this_file
    
  }
  
  return(    
    structure(csv_list, 
              class = "nowatchdata",
              zipfile = tools::file_path_sans_ext(zipfile),
              tz = tz
    ))
}