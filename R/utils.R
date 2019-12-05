
#' as_time
#' @description Converts Unix time to as.POSIXct 
#' @param x takes a unixtime and converts to as.POSIXct
#' @param origin defaults to 1970-1-1
#' @param tz defaults to "UTC"
#' @export
# Convert time in seconds to a POSIXct.
as_time <- function(x){
  as.POSIXct(x, origin = "1970-1-1", tz = "UTC")
}


#' prepend_time_column
#' @description Comlumn binds a time_column to the dataframe
#' @param data dataframe
#' @param timestart the start of the recording
#' @param hertz hertz in which the E4 data was recorded
#' @param tz The timezone, defaults to user timezone
#' @export 
#' @importFrom lubridate with_tz
prepend_time_column <- function(data, timestart, hertz, tz = Sys.timezone()){

  datetime <-  as_time(timestart) + (1/hertz) * (1:nrow(data) - 1)

  datetime <- with_tz(datetime, tz)

  out <- cbind(data.frame(DateTime = datetime), data)

  return(out)
}

#' rbind_e4
#' @description function to rowbind E4 data
#' @param data dataframe
#' @export
#' @importFrom dplyr bind_rows
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

#' pad_e4
#' @description function to combine several e4 files, and sets the length of the x-axis
#' @param x index of dataframe
#' @export
#' @importFrom dplyr left_join
pad_e4 <- function(x){

  interval <- as.numeric(median(diff(x$DateTime)))

  out <- data.frame(DateTime = seq(from = min(x$DateTime),
                                   to = max(x$DateTime),
                                   by = interval)
  )

left_join(out, x, by = "DateTime")
}







