#' Read E4 data
#' @description Reads in E4 data as a list (with EDA, HR, Temp, ACC, BVP, IBI as dataframes), and prepends timecolumns
#' @details This function reads in a zipfile as exported by Empatica Connect. Then it extracts the zipfiles in a temporary folder
#' and unzips the csv files in the temporary folder.
#'
#' The EDA, HR, BVP, and TEMP csv files have a similar structure in which the
#' starting time of the recording is read from the first row of the file (in unix time). The frequency of the measurements is read from the
#' second row of the recording (in Hz). Subsequently, the raw data is read from row three onward.
#'
#' The ACC csv file contain the acceleration of the Empatica E4 on the three axes x,y and z. The first row contains the starting
#' time of the recording in unix time. The second row contains the frequency of the measurements in Hz.
#' Subsequently, the raw x, y, and z data is read from row three onward.
#'
#' The IBI file has a different structure, the starting time in unix is in the first row, first column.
#' The firs column contins the number of seconds past since the start of the recording. The number of seconds past since the
#' start of the recording represent a heartbeat as derived from the algorithms from the photo plethysmogrophy sensor. The
#' second column contains the duration of the interval from one heartbeat to the next heartbeat.
#'
#' ACC.csv = 32 Hz
#' BVP.csv = 64 Hz
#' EDA.csv = 4 HZ
#' HR.csv = 1 HZ
#' TEMP.csv = 4 Hz
#'
#' Please also see the info.txt file provided in the zip file for additional information.
#'
#' The function returns an object of class "e4_data" with a prepended datetime columns
#' that defaults to user timezone. The object contains a list with dataframes from the physiological signals.
#'
#' @param zipfile A zip file as exported by the instrument
#' @param tz The timezone used by the instrument (defaults to user timezone).
#' @examples
#' library(wearables)
#' # read_e4()
#' @export
#' @importFrom R.utils countLines
#' @importFrom stats setNames
#' @importFrom utils read.table
read_e4 <- function(zipfile = NULL,
                    tz = Sys.timezone()) {
  if (is.null(zipfile)) {
    stop("Please enter the ZIP file with E4 data.")
  }

  # Temp directory: clean before use
  out_dir <- paste(sample(letters, 15), collapse = "")
  unlink(out_dir, recursive = TRUE)

  # Clean temp directory on exit
  on.exit(unlink(out_dir, recursive = TRUE))

  # Unzip files to temp directory
  unzip(zipfile, exdir = out_dir)


  check <- check_datafiles_filled(out_dir)
  if (!check) {
    warning(paste("One or more files in", zipfile, "is (nearly) empty."))
    return(NULL)
  }

  # Standard datasets and filenames
  datasets <- c("EDA", "ACC", "TEMP", "HR", "BVP")
  fns <- file.path(out_dir, paste0(datasets, ".csv"))


  # Get measurement frequency (on line 2)
  get_hertz <- function(x) {
    read.csv(x, skip = 1, nrow = 1, header = FALSE)[[1]]
  }

  hertz <- lapply(fns, get_hertz)
  names(hertz) <- datasets

  # Get time start (from line 1)
  get_timestart <- function(x) {
    read.csv(x, nrow = 1, header = FALSE)[[1]]
  }

  timestart <- lapply(fns, get_timestart)
  names(timestart) <- datasets

  # Read all datasets
  data <- lapply(fns, read.csv, header = FALSE, skip = 2)
  names(data) <- datasets

  for (d in datasets) {
    data[[d]] <- prepend_time_column(data[[d]], timestart[[d]], hertz[[d]],
      tz = tz
    )


    if (d %in% c("EDA", "TEMP", "HR", "BVP")) {
      names(data[[d]]) <- c("DateTime", d)
    } else if (d == "ACC") {
      names(data[[d]]) <- c("DateTime", "x", "y", "z")
    }
  }

  # Read IBI data separately (different format)
  ibi_file <- file.path(out_dir, "IBI.csv")
  ibi <- read.csv(ibi_file,
    stringsAsFactors = FALSE,
    header = FALSE, skip = 1
  )

  ibi_timestart <- get_timestart(file.path(out_dir, "IBI.csv"))
  timestart <- c(timestart, list(IBI = ibi_timestart))

  ibi <- cbind(data.frame(DateTime = as_time(ibi_timestart, tz = tz) + ibi[[1]]),
    seconds = ibi[[1]],
    IBI = ibi[[2]]
  )
  data <- c(data, list(IBI = ibi))

  # For ACC, add the geometric mean acceleration
  data$ACC$a <- sqrt(data$ACC$x^2 + data$ACC$y^2 + data$ACC$z^2) / 64


  # Is there a tags.csv file?
  tag_file <- file.path(out_dir, "tags.csv")
  if (file.exists(tag_file) && R.utils::countLines(tag_file) > 0) {
    data$tags <- stats::setNames(utils::read.table(tag_file), "DateTime")
    data$tags$DateTime <- as_time(data$tags$DateTime, tz = tz)
  } else {
    data$tags <- NULL
  }

  # Return data, store name of original file in the attributes, which we can read with:
  # attr(data, "zipfile")
  structure(data,
    class = "e4data",
    zipfile = tools::file_path_sans_ext(zipfile),
    tz = tz
  )
}
