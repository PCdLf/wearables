#' IBI analysis
#' 
#' Analysis of interbeat interval (IBI)
#' 
#' @param IBI IBI data, component of object (the number of seconds since the start of the recording) read with \code{\link{read_e4}}
#' @export
#' @importFrom RHRV CreateHRVData SetVerbose BuildNIHR FilterNIHR InterpolateNIHR 
#' @importFrom RHRV CreateTimeAnalysis CreateFreqAnalysis CalculatePowerBand
ibi_analysis <- function(IBI){
  
  # Select the heart beat positions in time. Use the amount of seconds since the start
  e4_hrv_data <- RHRV::CreateHRVData()
  e4_hrv_data <- RHRV::SetVerbose(e4_hrv_data, TRUE )
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
  suppressWarnings({
    e4_hrv_data <- RHRV::InterpolateNIHR(e4_hrv_data, freqhr = 4)  
    
    e4_hrv_data <- RHRV::CreateTimeAnalysis(e4_hrv_data, size = 300,
                                      interval = 7.8125)
  })
  
  # We typically have a lot of missing beats with wristbands, so frequency analysis is difficult.
  e4_hrv_data <- RHRV::CreateFreqAnalysis(e4_hrv_data)
  
  e4_hrv_data <- RHRV::CalculatePowerBand(e4_hrv_data , indexFreqAnalysis = 1,
                                    size = 300, shift = 30, type = "fourier",
                                    ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                                    LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )
  
  time <- e4_hrv_data$TimeAnalysis[[1]]
  freq <- e4_hrv_data$FreqAnalysis[[1]]
  
  list(
    time_analysis = time,
    #freq_analysis = freq,
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
