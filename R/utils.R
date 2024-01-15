#' as_time
#' @description Converts Unix time to as.POSIXct
#' @param x takes a unixtime and converts to as.POSIXct
#' @param tz timezone is set to UTC
#' @export
# Convert time in seconds to a POSIXct.
as_time <- function(x, tz = "UTC") {
  as.POSIXct(x, origin = "1970-1-1", tz = tz)
}


#' prepend_time_column
#' @description Column binds a time_column to the dataframe
#' @param data dataframe
#' @param timestart the start of the recording
#' @param hertz hertz in which the E4 data was recorded
#' @param tz The timezone, defaults to user timezone
#' @export
#' @importFrom lubridate with_tz
prepend_time_column <- function(data, timestart, hertz, tz = Sys.timezone()) {
  datetime <- as_time(timestart) + (1 / hertz) * (1:nrow(data) - 1)

  datetime <- with_tz(datetime, tz)

  out <- cbind(data.frame(DateTime = datetime), data)

  return(out)
}


#' pad_e4
#' @description function to combine several e4 files, and sets the length of the x-axis
#' @param x index of dataframe
#' @export
#' @importFrom dplyr left_join
#' @importFrom stats median
#' @importFrom utils unzip
#' @importFrom utils read.csv
pad_e4 <- function(x) {
  interval <- as.numeric(median(diff(x$DateTime)))

  out <- data.frame(DateTime = seq(
    from = min(x$DateTime),
    to = max(x$DateTime),
    by = interval
  ))

  left_join(out, x, by = "DateTime")
}


# Are all provided data files filled?
# Returns FALSE if one or more files have 1 or 0 lines of data.
check_datafiles_filled <- function(path) {
  datasets <- c("EDA", "ACC", "TEMP", "HR", "BVP", "IBI")
  fns <- file.path(path, paste0(datasets, ".csv"))

  nrows <- sapply(fns, R.utils::countLines)

  all(nrows > 1)
}


#' Create an Empty Time List
#'
#' This function generates a list named 'time' with predefined fields, all set to NA.
#' The fields included are related to time-based measurements.
#'
#' @return A named list with all fields set to NA.
#' @export
#' @examples
#' empty_time_list <- create_empty_time_list()
#' str(empty_time_list)
create_empty_time_list <- function() {
  time_names <- c("size", "SDNN", "SDANN", "SDNNIDX", "pNN50", "SDSD", 
                  "rMSSD", "IRRR", "MADRR", "TINN", "HRVi")
  
  # Create 'time' list with NA values
  time_empty_list <- setNames(vector("list", length(time_names)), time_names)
  
  # Assign NA to all numeric fields in 'time'
  time_empty_list[] <- lapply(time_empty_list, function(x) as.numeric(NA))
  
  return(time_empty_list)
}


#' Create an Empty Frequency List
#'
#' This function generates a list named 'freq' with predefined fields for frequency measurements. 
#' Most fields are initialized with NA values, except for 'type' which is set to "fourier".
#'
#' @return A named list with frequency measurement fields, mostly set to NA.
#' @export
#' @examples
#' empty_freq_list <- create_empty_freq_list()
#' str(empty_freq_list)
create_empty_freq_list <- function() {
  
  # Names for the 'freq' list
  freq_names <- c("HRV", "ULF", "VLF", "LF", "HF", "LFHF", "Time", 
                  "size", "shift", "sizesp", "type", "ULFmin", "ULFmax", 
                  "VLFmin", "VLFmax", "LFmin", "LFmax", "HFmin", "HFmax")
  
  # Create 'freq' list with NA values
  freq_empty_list <- setNames(vector("list", length(freq_names)), freq_names)
  
  # Assign NA to all numeric fields in 'freq' and set 'type' to "fourier"
  freq_empty_list[-which(names(freq_empty_list) == "type")] <- lapply(freq_empty_list[-which(names(freq_empty_list) == "type")], function(x) as.numeric(NA))
  freq_empty_list[["type"]] <- "fourier" 
  
  return(freq_empty_list)
}

