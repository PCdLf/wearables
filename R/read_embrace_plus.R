#' Extract time column from Embrace Plus data
#' @description Extracts the time column from Embrace Plus data
#' @param start_time start time of the recording in seconds
#' @param sampling_freq sampling frequency of the recording
#' @param len_list length of the list
#' @param tz timezone
#' @keywords internal
#' @noRd
get_timestamp_column <- function(start_time, sampling_freq, len_list, tz) {
  start_time_ns <- start_time * 1000
  start_timestamp <- as.POSIXct(start_time_ns / 1e9, origin = "1970-01-01", tz = tz)
  
  # Calculate end_timestamp based on the length of the list and sampling frequency
  end_timestamp <- start_timestamp + as.difftime(len_list / sampling_freq, units = "secs")
  
  # Generate a range of timestamps from start to end with the given frequency
  timestamp_column <- seq(from = start_timestamp, to = end_timestamp, by = 1 / sampling_freq)
  timestamp_df <- data.frame(DateTime = timestamp_column)
  
  # Convert 'timestamp' column back to Unix timestamp in seconds
  timestamp_df$unix_timestamp <- as.numeric(as.POSIXct(timestamp_df$DateTime, origin = "1970-01-01", tz = "UTC"))
  
  # Drop last row
  if (nrow(timestamp_df) > len_list) {
    timestamp_df <- timestamp_df[-nrow(timestamp_df), ]
  }
  
  return(timestamp_df)
}

#' Create dataframe for psychological factors
#' @description Creates a dataframe for psychological factors
#' @param data list of dataframes
#' @param type type of data to extract
#' @param file filename of the original data
#' @param vars variables to extract
#' @param tz timezone
#' @param timestamp_start vector of 3 elements, containing the names of the columns 
#'   in the data that contain the start time of the recording.
#' @keywords internal
#' @noRd
create_dataframes <- function(data, type, file, vars = c("x", "y", "z"), 
                              timestamp_start = NULL, tz) {
  
  if (!all(vars %in% names(data[[type]]))) {
    stop(sprintf("vars must be in the data, vars found are: %s", 
                 paste0(names(data[[type]]), collapse = ", ")))
  }
  
  if (!type %in% names(data)) {
    stop(sprintf("type must be in the data, types found are: %s", 
                 paste0(names(data), collapse = ", ")))
  }
  
  df <- data.frame()
  
  if (length(data[[type]][[vars[1]]]) == 0) {
    cli_alert_warning(sprintf("Empty %s data in %s", type, file))
  } else {
    for (var in vars) {
      if (length(df) == 0) {
        df <- data.frame(x = data[[type]][[var]])
        names(df) <- c(var)
      }
      df[[var]] <- data[[type]][[var]]
    }
    
    if (!is.null(timestamp_start)) {
      if (length (timestamp_start) != 3) {
        stop("timestamp_start must be a vector of length 3")
      }
      
      timestamp_df <- get_timestamp_column(data[[type]][[timestamp_start[1]]], 
                                           data[[type]][[timestamp_start[2]]], 
                                           length(data[[type]][[timestamp_start[3]]]),
                                           tz = tz)
      
      df <- cbind(df, timestamp_df)
    }
  }
  return(df)
}

#' Read Embrace Plus data
#' @description Reads in Embrace Plus data as a list (with EDA, HR, Temp, ACC, BVP, IBI as dataframes), and prepends timecolumns
#' @details This function reads in a zipfile as exported by Embrace Plus. Then it extracts the zipfiles in a temporary folder
#' and unzips them in the same temporary folder.
#'
#' The unzipped files are avro files, and are read in with using `sparklyr`, which sets up a local Spark cluster.
#'
#' The function returns an object of class "embrace_plus_data" with a prepended datetime columns.
#' The object contains a list with dataframes from the physiological signals.
#'
#' @param zipfile A zip file as exported by the instrument
#' @param tz The timezone used by the instrument (defaults to user timezone).
#' @examples
#' library(wearables)
#' # read_embrace_plus("yourpathtohezipfile.zip")
#' @export
#' @import sparklyr
#' @import cli
#' @importFrom dplyr pull
read_embrace_plus <- function(zipfile,
                              tz = Sys.timezone()) {
  
  # Check for already installed Spark versions
  # if none available, install the latest version
  if (nrow(spark_available_versions()) == 0) {
    cli_alert_info("Installing Spark")
    spark_install(version = tail(spark_available_versions(), 1)$spark)
  }
  
  # Open a local Spark connection
  # Attach avro package to be able to read in avro files
  cli_alert_info("Connecting to local Spark cluster")
  sc <- spark_connect(master = "local",
                      version = tail(spark_available_versions(), 1)$spark,
                      packages = "org.apache.spark:spark-avro_2.12:3.5.0")
  cli_alert_success("Connected!")
  
  # Extract files to a temporary folder
  path <- paste0(tempdir(), "/extracted")
  
  # if path exists, remove content
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE)
  }
  
  unzip(zipfile = zipfile, 
        exdir = path)
  
  avro_files <- list.files(path, recursive = TRUE, pattern = "[.]avro$", full.names = TRUE)
  
  cli_alert_info("About to start processing {length(avro_files)} avro file{?s}")
  
  i <- 0
  cli_progress_step("Processed {i}/{length(avro_files)} {qty(i)}file{?s}",
                    msg_done = "Completed data reading and processing",
                    spinner = TRUE)
  
  for (file in avro_files) {
    
    # Read each avro file into a spark dataframe
    record <-
      spark_read_avro(
        sc,
        "embraceplus",
        file,
        repartition = FALSE,
        memory = FALSE,
        overwrite = TRUE
      )
    
    # Pull the data from the Spark cluster
    raw_data <- pull(record, rawData)
    raw_data <- raw_data[[1]]
    
    acc_data <- create_dataframes(raw_data, 
                                  type = "accelerometer", 
                                  file, 
                                  timestamp_start = c("timestampStart", "samplingFrequency", "x"),
                                  tz = tz)
    # For ACC, add the geometric mean acceleration
    acc_data$a <- sqrt(acc_data$x^2 + acc_data$y^2 + acc_data$z^2) / 64
    
    gy_data <- create_dataframes(raw_data, 
                                 type = "gyroscope", 
                                 file, 
                                 timestamp_start = c("timestampStart", "samplingFrequency", "x"),
                                 tz = tz)
    
    eda_data <- create_dataframes(raw_data, 
                                  type = "eda",
                                  vars = "values",
                                  file, 
                                  timestamp_start = c("timestampStart", "samplingFrequency", "values"),
                                  tz = tz)
    
    temp_data <- create_dataframes(raw_data, 
                                   type = "temperature",
                                   vars = "values",
                                   file, 
                                   timestamp_start = c("timestampStart", "samplingFrequency", "values"),
                                   tz = tz)
    
    bvp_data <- create_dataframes(raw_data,
                                  type = "bvp",
                                  vars = "values",
                                  file, 
                                  timestamp_start = c("timestampStart", "samplingFrequency", "values"),
                                  tz = tz)
    
    steps_data <- create_dataframes(raw_data,
                                    type = "steps",
                                    vars = "values",
                                    file, 
                                    timestamp_start = c("timestampStart", "samplingFrequency", "values"),
                                    tz = tz)
    
    systolic_peaks_data <- create_dataframes(raw_data,
                                             type = "systolicPeaks",
                                             vars = "peaksTimeNanos",
                                             file)
    
    
    this_avro_list <- list(
      ACC = acc_data,
      GY = gy_data,
      EDA = eda_data,
      TEMP = temp_data,
      BVP = bvp_data,
      steps = steps_data,
      systolic_peaks = systolic_peaks_data
    )
    
    if (exists("avro_list")) {
      avro_list <- append(avro_list, list(this_avro_list))
    } else {
      avro_list <- list()
      avro_list[[1]] <- this_avro_list
    }
    
    i <- i + 1
    cli_progress_update()
    
    if(i == length(avro_files)) {
      cli_progress_done()
    }
    
  }
  
  # Disconnect from the Spark cluster
  spark_disconnect(sc)
  
  return(
    structure(avro_list, 
              class = "embraceplusdata",
              zipfile = tools::file_path_sans_ext(zipfile),
              tz = tz
    )
  )
}
