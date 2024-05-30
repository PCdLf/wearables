#' Read Nowatch data
#' @description Reads in Nowatch data as a list, and prepends timecolumns
#' @details This function reads in a zipfile with files exported by the Nowatch instrument,
#' or a folder with the unzipped files. The files are expected to be csv files.
#'
#' The unzipped files are csv files.
#'
#' The function returns an object of class "nowatchdata" with a prepended datetime columns.
#' The object contains a list with dataframes from the physiological signals.
#'
#' @param zipfile A zip file as exported by the instrument. Only aggregated data supported.
#' @param folder A folder with the unzipped files. If this is provided, the zipfile is not used.
#' @param tz The timezone used by the instrument (defaults to user timezone).
#' @examples
#' \dontrun{
#'  library(wearables)
#'  read_nowatch("yourpathtohezipfile.zip")
#'  read_nowatch(folder = "/path/to/folder/with/files")
#' }
#' @export
#' @import cli
read_nowatch <- function(zipfile = NULL,
                         folder = NULL,
                         tz = Sys.timezone()) {
  
  # Check if zipfile or folder is provided
  if (is.null(zipfile) && is.null(folder)) {
    cli_abort("Please provide a zipfile or a folder")
  }
  
  # Check if zipfile exists
  if (!is.null(zipfile) && !file.exists(zipfile)) {
    cli_abort("File does not exist")
  }
  
  # Check if folder exists
  if (!is.null(folder) && !dir.exists(folder)) {
    cli_abort("Folder does not exist")
  }
  
  # Unzip the files
  if (!is.null(zipfile)) {
    csv_files <- unzip_files(zipfile, "csv")
  }
  
  if (!is.null(folder)) {
    csv_files <- list.files(folder, full.names = TRUE)
  }
  
  # Get the content before .csv and after the last /
  dataset_names <- gsub(".csv", "", basename(csv_files))
  dataset_names <- toupper(dataset_names)
  dataset_names <- gsub("ACTIVITY_TYPE", "ACT", dataset_names)
  dataset_names <- gsub("ACTIVITY_CADENCE", "CAD", dataset_names)
  dataset_names <- gsub("ACTIVITY_COUNT", "COUNT", dataset_names)
  dataset_names <- gsub("TEMPERATURE", "TEMP", dataset_names)
  dataset_names <- gsub("CORTISOLLEVELS", "CORTL", dataset_names)
  dataset_names <- gsub("HEARTBEATS", "HB", dataset_names)
  dataset_names <- gsub("HEART_RATE", "HR", dataset_names)
  dataset_names <- gsub("RESTING_HEART_RATE", "RHR", dataset_names)
  dataset_names <- gsub("RESPIRATION_RATE", "RR", dataset_names)
  dataset_names <- gsub("SLEEP_SESSION", "SLEEP", dataset_names)
  dataset_names <- gsub("STRESS_LEVEL", "STRESS", dataset_names)
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