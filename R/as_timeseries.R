#' Convert an E4 data stream to a timeseries
#' @description Creates an xts object indexed by time
#' @param data A dataframe, subelements of list as output by read_e4 function
#' @param index Which column (integer) to use as the data in the timeseries. Default: 2.
#' @param name_col Column name to give to the timeseries data.
#'
#' @export
#' @importFrom xts xts
as_timeseries <- function(data, index = 2, name_col = "V1") {
  tdata <- pad_e4(data)

  panel <- xts(tdata[[index]], order.by = tdata[[1]])
  colnames(panel) <- name_col
  return(panel)
}
