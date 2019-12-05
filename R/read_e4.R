#' Read E4 data
#' @description Reads in E4 data as a list (with EDA, HR, Temp, ACC, BVP, IBI as dataframes), and prepends timecolumns
#' @details Details here
#' @param zipfile A zip file as exported by the instrument
#' @param tz The timezone used by the instrument (defaults to user timezone).
#'
#' @export
read_e4 <- function(zipfile = NULL,
                    tz = Sys.timezone()){

  if(is.null(zipfile)){
    stop("Please enter the ZIP file with E4 data.")
  }

  # Temp directory: clean before use
  out_dir <- paste(sample(letters,15), collapse = "")
  unlink(out_dir, recursive = TRUE)

  # Clean temp directory on exit
  on.exit(unlink(out_dir, recursive = TRUE))

  # Unzip files to temp directory
  unzip(zipfile, exdir = out_dir)

  # Standard datasets and filenames
  datasets <- c("EDA","ACC","TEMP","HR","BVP")
  fns <- file.path(out_dir, paste0(datasets, ".csv"))

  # Get measurement frequency (on line 2)
  get_hertz <- function(x){
    read.csv(x, skip=1, nrow=1, header=FALSE)[[1]]
  }

  hertz <- lapply(fns, get_hertz)
  names(hertz) <- datasets

  # Get time start (from line 1)
  get_timestart <- function(x){
    read.csv(x, nrow=1, header=FALSE)[[1]]
  }

  timestart <- lapply(fns, get_timestart)
  names(timestart) <- datasets

  # Read all datasets
  data <- lapply(fns, read.csv, header=FALSE, skip = 2)
  names(data) <- datasets

  for(d in datasets){

    data[[d]] <- prepend_time_column(data[[d]], timestart[[d]], hertz[[d]],
                                     tz = tz)
  }

  # Read IBI data separately (different format)
  ibi <- read.csv(file.path(out_dir, "IBI.csv"),
                  stringsAsFactors = FALSE,
                  header = FALSE, skip = 1)

  ibi_timestart <- get_timestart(file.path(out_dir, "IBI.csv"))
  timestart <- c(timestart, list(IBI = ibi_timestart))

  ibi <- cbind(data.frame(DateTime = as_time(ibi_timestart) + ibi[[1]]),
               ibi[[2]], ibi[[1]]
  )
  data <- c(data, list(IBI = ibi))


  # For ACC, add the geometric mean acceleration
  data$ACC$V4 <- sqrt(data$ACC$V1^2 + data$ACC$V2^2 + data$ACC$V3^2)

class(data) <- "e4data"
return(data)

}
