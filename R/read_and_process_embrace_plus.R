#' Read, process and feature extraction of Embrace Plus data
#' @description Reads the raw ZIP file using `read_embrace_plus`,
#'   performs analyses with `eda_analysis`.
#' @param zipfile zip file with embrace plus data to be read
#' @param tz timezone where data were recorded (default system timezone)
#' @return An object with processed data and analyses, object of class 'embrace_plus_analysis'.
#' @rdname read_and_process_embrace_plus
#' @export
read_and_process_embrace_plus <- function(zipfile, tz = Sys.timezone()) {
  data <- read_embrace_plus(zipfile, tz)
  data <- rbind_embrace_plus(data)
  
  if (is.null(data)) {
    return(NULL)
  } else {
    flog.info("Raw data read and converted.")
    process_embrace_plus(data)
  }
}

#' @rdname read_and_process_e4
#' @export
#' @param data object from read_e4 function
process_embrace_plus <- function(data) {

  eda_filt <- process_eda(data$EDA)
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
      data_summary = list(
        ACC = acc_summary,
        TEMP = temp_summary,
        EDA = eda_summary,
        peaks = peaks_summary
      ),
      eda_peaks = eda_peaks,
      eda_bin = eda_bin_pred,
      eda_mc = eda_mc_pred
    ),
    class = "embrace_plus_analysis"
  )
}
