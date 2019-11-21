
# Convert time in seconds to a POSIXct.
as_time <- function(x){
  as.POSIXct(x, origin = "1970-1-1", tz = "UTC")
}


#' @importFrom lubridate with_tz
prepend_time_column <- function(data, timestart, hertz, tz = Sys.timezone()){

  datetime <-  as_time(timestart) + (1/hertz) * (1:nrow(data) - 1)

  datetime <- lubridate::with_tz(datetime, tz)

  out <- cbind(data.frame(DateTime = datetime), data)

  return(out)
}


#' @importFrom dplyr bind_rows
rbind_e4 <- function(data){

  out <- list()

  nms <- names(data[[1]])

  for(name in nms){

    # retrieve data
    dat <- lapply(data, "[[", name)
    out[[name]] <- dplyr::bind_rows(dat)

  }

out
}

#' @importFrom dplyr left_join
pad_e4 <- function(x){

  interval <- as.numeric(median(diff(x$DateTime)))

  out <- data.frame(DateTime = seq(from = min(x$DateTime),
                                   to = max(x$DateTime),
                                   by = interval)
  )

dplyr::left_join(out, x, by = "DateTime")
}







