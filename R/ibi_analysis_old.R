#' IBI analysis (Old version)
#'
#' This function is the older version of IBI (interbeat interval) analysis. It has been replaced by 
#'  \code{\link{ibi_analysis}}. This version remains available for compatibility, but we want to deprecate
#'  the function in future versions of the package. 
#'  
#'  This function analyzes IBI data, focusing on time and frequency domain measures.
#'  Note that the function imports functions from the RHRV package for HRV (Heart Rate Variability)
#'  analysis. Also note that measurements on the wrist are now more often referred to as PRV (Pulse
#'  Rate Variability), see for instance
#'   https://jphysiolanthropol.biomedcentral.com/articles/10.1186/s40101-020-00233-x.
#'
#' @param IBI IBI data, a dataframe with columns including DateTime and seconds since the start
#' of the recording. This data can be read with read with \code{\link{read_e4}}
#' 
#' @return 
#' A list containing time analysis, frequency analysis (commented out in this version),
#' and a summary of the results including time domain measures (SDNN, pNN50, etc.), 
#' frequency domain measures (HF, LF, LFHF, etc.), and the number of original and accepted beats.
#'    
#' @details
#' The function performs several steps in analyzing IBI data:
#' 1. Create an HRV data structure using RHRV.
#' 2. Filter and preprocess the data, including handling of missing values and erroneous readings.
#' 3. Perform time domain and frequency domain analyses.
#' 4. Return a comprehensive list of results, including statistical summaries of the HRV measures.
#'
#' @note This function is deprecated and will be removed in future versions. Users should migrate to 
#'       \code{\link{ibi_analysis}} for ongoing and future analyses.
#'
#' @seealso \code{\link{ibi_analysis}} for the current version of IBI analysis.
#' 
#' @examples
#' \dontrun{
#' zip_path <- system.file("extdata", "1635148245_A00204.zip", package = "wearables")
#' Assuming "IBI_data" is your interbeat interval data
#' result <- read_e4("path to your file") 
#' print(result$IBI)
#' }
#'
#' @export
#' @importFrom RHRV CreateHRVData SetVerbose BuildNIHR FilterNIHR InterpolateNIHR CreateTimeAnalysis CreateFreqAnalysis CalculatePowerBand
ibi_analysis_old <- function(IBI) {
  
  .Deprecated("ibi_analysis")
  
  # Select the heart beat positions in time. Use the amount of seconds since the start
  e4_hrv_data <- RHRV::CreateHRVData()
  e4_hrv_data <- RHRV::SetVerbose(e4_hrv_data, TRUE)
  e4_hrv_data$datetime <- as.POSIXlt(IBI$DateTime)[1]

  # There is no 0 added to the Empatica E4 seconds column, therefore, slight deviations
  # with RHRV are possible. To match RHRV outcome, add 0 to the dataframe.
  # Reason for not adding the 0 is that Empatica does not contain a valid first
  # RR interval from the start of the study.
  e4_hrv_data$Beat <- data.frame(Time = IBI$seconds)

  n_beats_original <- nrow(e4_hrv_data$Beat)

  # Then build the non interpolated heart rate series
  e4_hrv_data <- RHRV::BuildNIHR(e4_hrv_data)

  # Pay attention that we don't need the inter-beat-intervals as RHRV does not know how to handle these
  # as there are so much missing values in there.

  # Remove too short RR intervals or missed beats
  # This also provides the number of accepted beats

  e4_hrv_data <- RHRV::FilterNIHR(e4_hrv_data)

  n_beats_accepted <- nrow(e4_hrv_data$Beat)

  # Note that it is not necessary to specify freqhr since it matches with
  # the default value: 4 Hz
  # suppressWarnings({
    e4_hrv_data <- RHRV::InterpolateNIHR(e4_hrv_data, freqhr = 4)

    e4_hrv_data <- RHRV::CreateTimeAnalysis(e4_hrv_data,
      size = 300,
      interval = 7.8125
    )
  # })

  # We typically have a lot of missing beats with wristbands, so frequency analysis is difficult.
  e4_hrv_data <- RHRV::CreateFreqAnalysis(e4_hrv_data)

  e4_hrv_data <- RHRV::CalculatePowerBand(e4_hrv_data,
    indexFreqAnalysis = 1,
    size = 300, shift = 30, type = "fourier",
    ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
    LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4
  )

  time <- e4_hrv_data$TimeAnalysis[[1]]
  freq <- e4_hrv_data$FreqAnalysis[[1]]

  list(
    time_analysis = time,
    # freq_analysis = freq,
    summary = list(
      time = list(
        SDNN = time$SDNN,
        pNN50 = time$pNN50,
        SDSD = time$SDSD,
        rMSSD = time$rMSSD,
        HRVi = time$HRVi,
        SDANN = time$SDANN,
        TINN = time$TINN
      ),
      frequency = list(
        HF = mean(freq$HF),
        LF = mean(freq$LF),
        LFHF = mean(freq$LFHF),
        VLF = mean(freq$VLF),
        ULF = mean(freq$ULF)
      ),
      beats = list(
        beats_original = n_beats_original,
        beats_accepted = n_beats_accepted
      )
    )
  )
}
