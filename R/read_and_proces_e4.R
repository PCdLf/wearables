#' Read, process and feature extraction of E4 data
#' @description Reads the raw ZIP file using `read_e4`,
#'   performs analyses with `ibi_analysis` and `eda_analysis`.
#' @param zipfile zip file with e4 data to be read
#' @param tz timezone where data were recorded (default system timezone)
#' @return An object with processed data and analyses, object of class 'e4_analysis'.
#' @rdname read_and_process_e4
#' @importFrom utils write.csv2
#' @importFrom padr thicken
#' @importFrom dplyr all_of
#' @export
read_and_process_e4 <- function(zipfile, tz = Sys.timezone()) {
  data <- read_e4(zipfile, tz)

  if (is.null(data)) {
    return(NULL)
  } else {
    flog.info("Raw data read and converted.")
    process_e4(data)
  }
}


# Join EDA binary classifier output to a dataset, based on rounded 5sec intervals.
# Rename 'label' to 'quality_flag'
join_eda_bin <- function(data, eda_bin) {
  if (nrow(data) == 0) {
    data$quality_flag <- integer(0)
    return(data)
  }

  # padr::thicken(data, interval = "5 sec") %>%
  #   dplyr::left_join(eda_bin, by = c("DateTime_5_sec" = "id")) %>%
  #   dplyr::select(-dplyr::all_of("DateTime_5_sec")) %>%
  #   dplyr::rename(quality_flag = "label")

  # Updated function, sometimes the quality_flag received NA if the function
  # did not floor the date properly
  padr::thicken(data, interval = "5 sec") %>%
    dplyr::left_join(
      mutate(eda_bin,
        DateTime_5_sec = lubridate::floor_date(id, "5 seconds")
      ),
      by = "DateTime_5_sec"
    ) %>%
    dplyr::select(-dplyr::all_of(c("DateTime_5_sec", "id"))) %>%
    dplyr::rename(quality_flag = "label")
}




#' @rdname read_and_process_e4
#' @export
#' @param data object from read_e4 function
process_e4 <- function(data) {
  suppressMessages({
    suppressWarnings({
      ibi <- ibi_analysis(data$IBI)
    })
  })
  flog.info("IBI data analyzed.")

  eda_filt <- wearables::process_eda(data$EDA)
  flog.info("EDA data filtered.")

  eda_peaks <- find_peaks(eda_filt)
  flog.info("Peak detection complete.")

  eda_feat <- compute_features2(eda_filt)
  flog.info("EDA Features computed")

  eda_bin_pred <- predict_binary_classifier(eda_feat)
  eda_mc_pred <- predict_multiclass_classifier(eda_feat)
  flog.info("Model predictions generated, artifacts classified.")

  # Add quality flags to data
  eda_filt <- join_eda_bin(eda_filt, eda_bin_pred)
  eda_peaks <- join_eda_bin(eda_peaks, eda_bin_pred)

  # Time range of the data
  r <- range(eda_filt$DateTime)
  time_range <- as.numeric(difftime(r[2], r[1], units = "min"))

  # Additional summaries
  hr_summary <- list(
    HR_mean = mean(data$HR$HR),
    HR_median = median(data$HR$HR),
    HR_min = min(data$HR$HR),
    HR_max = max(data$HR$HR),
    HR_sd = sd(data$HR$HR)
  )

  temp_summary <- list(
    TEMP_mean = mean(data$TEMP$TEMP),
    TEMP_median = median(data$TEMP$TEMP),
    TEMP_min = min(data$TEMP$TEMP),
    TEMP_max = max(data$TEMP$TEMP),
    TEMP_sd = sd(data$TEMP$TEMP)
  )

  acc_summary <- list(
    ACC_mean = mean(data$ACC$a),
    ACC_median = median(data$ACC$a),
    ACC_min = min(data$ACC$a),
    ACC_max = max(data$ACC$a),
    ACC_sd = sd(data$ACC$a)
  )

  eda_clean <- dplyr::filter(eda_filt, .data$quality_flag == 1)

  if (nrow(eda_clean) > 0) {
    eda_summary <- list(
      EDA_clean_mean = mean(eda_clean$EDA),
      EDA_clean_median = median(eda_clean$EDA),
      EDA_clean_min = min(eda_clean$EDA),
      EDA_clean_max = max(eda_clean$EDA),
      EDA_clean_sd = sd(eda_clean$EDA)
    )
  } else {
    eda_summary <- list(
      EDA_clean_mean = NA,
      EDA_clean_median = NA,
      EDA_clean_min = NA,
      EDA_clean_max = NA,
      EDA_clean_sd = NA
    )
  }

  pks_clean <- dplyr::filter(eda_peaks, .data$quality_flag == 1)

  if (nrow(eda_clean) > 0) {
    peaks_summary <- list(
      peaks_clean_sum = nrow(pks_clean),
      peaks_clean_per_min = nrow(pks_clean) / time_range,
      peaks_clean_mean_auc = mean(pks_clean$AUC),
      peaks_clean_mean_amp = mean(pks_clean$amp)
    )
  } else {
    peaks_summary <- list(
      peaks_clean_sum = 0,
      peaks_clean_mean_auc = NA,
      peaks_clean_mean_amp = NA
    )
  }


  structure(
    list(
      data = data,
      ibi = ibi,
      data_summary = list(
        ACC = acc_summary,
        TEMP = temp_summary,
        HR = hr_summary,
        EDA = eda_summary,
        peaks = peaks_summary
      ),
      eda_peaks = eda_peaks,
      eda_bin = eda_bin_pred,
      eda_mc = eda_mc_pred
    ),
    class = "e4_analysis"
  )
}



#' Output folder
#'
#' Create output folder for E4 analysis results
#'
#' @param obj e4 analysis object
#' @param out_path output folder
#' @export
create_e4_output_folder <- function(obj, out_path = ".") {
  stopifnot(inherits(obj, "e4_analysis"))

  # read_e4 stored the zipname as an attribute
  zipname <- basename(attr(obj$data, "zipfile"))

  # Output goes to a folder with the zipfile name
  out_folder <- file.path(out_path, zipname)
  dir.create(out_folder)

  return(invisible(out_folder))
}


#' Write CSV files of the output
#' @description Slow!
#' @param obj e4 analysis object
#' @param out_path output folder
#' @export
write_processed_e4 <- function(obj, out_path = ".") {
  stopifnot(inherits(obj, "e4_analysis"))

  out_folder <- create_e4_output_folder(obj, out_path)

  # write.csv(obj$data$ , file.path(out_folder, ...)

  file_out <- function(data, name) {
    write.csv2(data, file.path(out_folder, name), row.names = FALSE)
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
  file_out(ibi_s, "IBI_SummaryPars.csv")

  # EDA model predictions
  file_out(obj$eda_bin, "EDA_binary_prediction.csv")
  file_out(obj$eda_mc, "EDA_multiclass_prediction.csv")
}
