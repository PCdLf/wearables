#' Analysis of interbeat interval (IBI)
#' @param IBI IBI data, component of object read with \code{\link{read_e4}}
#' @export
#' @importFrom RHRV CreateHRVData SetVerbose BuildNIHR FilterNIHR InterpolateNIHR 
#' @importFrom RHRV CreateTimeAnalysis CreateFreqAnalysis CalculatePowerBand
ibi_analysis <- function(IBI){
  
  # Select the heart beat positions in time. Use the amount of seconds since the start
  e4_hrv_data <- RHRV::CreateHRVData()
  e4_hrv_data <- RHRV::SetVerbose(e4_hrv_data, TRUE )
  e4_hrv_data$datetime <- as.POSIXlt(IBI$DateTime)[1]
  
  e4_hrv_data$Beat <- data.frame(Time = IBI$seconds)
  
  # Then build the non interpolated heart rate series
  e4_hrv_data <- RHRV::BuildNIHR(e4_hrv_data)
  
  # Pay attention that we don't need the inter-beat-intervals as RHRV does not know how to handle these
  # as there are so much missing values in there.
  
  # Remove too short RR intervals or missed beats
  # This also provides the number of accepted beats
  n_beats_original <- nrow(e4_hrv_data$Beat)
  
  e4_hrv_data <- RHRV::FilterNIHR(e4_hrv_data)
  
  n_beats_accepted <- nrow(e4_hrv_data$Beat)
  
  # Note that it is not necessary to specify freqhr since it matches with
  # the default value: 4 Hz
  suppressWarnings({
    e4_hrv_data <- RHRV::InterpolateNIHR(e4_hrv_data, freqhr = 4)  
    
    e4_hrv_data <- RHRV::CreateTimeAnalysis(e4_hrv_data, size = 100,
                                      interval = 7.8125)
  })
  
  # We have a lot of missing beats, so frequency analysis is difficult.
  e4_hrv_data <- RHRV::CreateFreqAnalysis(e4_hrv_data)
  
  e4_hrv_data <- RHRV::CalculatePowerBand(e4_hrv_data , indexFreqAnalysis = 1,
                                    size = 300, shift = 30, type = "fourier",
                                    ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                                    LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )
  
  time <- e4_hrv_data$TimeAnalysis[[1]]
  freq <- e4_hrv_data$FreqAnalysis[[1]]
  
  list(
    time_analysis = time,
    freq_analysis = freq,
    summary = list(
      time = list(
        SDNN = time$SDNN,
        pNN50 = time$pNN50,
        SDSD = time$SDSD,
        rMSSD = time$rMSSD
      ),
      frequency = list(
        HF = mean(freq$HF),
        LF = mean(freq$LF),
        HFLF = mean(freq$HFLF),
        VLF = mean(freq$VLF)
      ),
      beats = list(
        beats_original = n_beats_original,
        beats_accepted = n_beats_accepted
      )
    )
  )
  
  
  
  
  
}