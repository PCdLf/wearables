library(wearables)
wearables::read_and_process_e4("C:/repos/BVI/Sensor_data/Peter 2024/Test_empty_ibi/1635148245_A00204_1.zip")
test <- wearables::read_and_process_e4("C:/repos/BVI/Sensor_data/Peter 2024/Test_empty_ibi/1635148245_A00204_1.zip")
test <- wearables::read_and_process_e4("C:/repos/BVI/Sensor_data/Peter 2024/Test_empty_ibi/1635148245_A00204_11_interpolate error - 4 row IBI.zip")
test <- wearables::read_and_process_e4("C:/repos/BVI/Sensor_data/Peter 2024/Test_empty_ibi/1635148245_A00204_2.zip")
test <- wearables::read_and_process_e4("C:/repos/BVI/Sensor_data/Peter 2024/Test_empty_ibi/1635148245_A00204_1.zip")

# What will it say when an IBI is completely empty?
wearables::read_and_process_e4("C:/repos/BVI/Sensor_data/Peter 2024/Test_empty_ibi/1636449414_empty_hr_ibi.zip")
# Result --> It will throw a NULL, stop execution and gives a warning message.
# In the batch files it will skip the file and move on.

##-- Good file --##
# What happens if a file is just OK?
test <- wearables::read_e4("C:/repos/BVI/Sensor_data/Peter 2024/Test_empty_ibi/1635148245_A00204_1.zip")

##-- Bad file - 4 rows, only 1 accepted, InterpolationFunction does not run --##
# What happens with 4 rows of IBI data that after filtering provide only one valid row?
test <- wearables::read_e4("C:/repos/BVI/Sensor_data/Peter 2024/Test_empty_ibi/1635148245_A00204_11_interpolate error - 4 row IBI.zip")
# The Interpolate function will throw an error. 

##-- Bad file - 1 row, 0 accepted, BuildFunction does not run --##
# What happens with 1 row of IBI data?
test <- wearables::read_e4("C:/repos/BVI/Sensor_data/Peter 2024/Test_empty_ibi/1635148245_A00204_11_interpolate error - 1 row IBI.zip")
# Result --> The read_e4 function will work, but the BuildNIHR will then throw an error. 

test$IBI

e4_hrv_data <- RHRV::CreateHRVData()
e4_hrv_data <- RHRV::SetVerbose(e4_hrv_data, TRUE)
e4_hrv_data$datetime <- as.POSIXlt(test$IBI$DateTime)[1]
test$IBI$seconds
test$IBI$IBI
e4_hrv_data$Beat <- data.frame(Time = test$IBI$seconds)
n_beats_original <- nrow(e4_hrv_data$Beat)

# Check whether the code will run with NA values for time and freq
# The first error

# Define the error handling function
handle_hrv_error <- function(e) {
  warning("Error in BuildNIHR or InterpolateNIHR: ", e$message)
  list(time = create_empty_time_list(), 
       freq = create_empty_freq_list(),
       n_beats_accepted = 0)
}

# Main processing with error handling
result_hrv_processing <- tryCatch({
  
  e4_hrv_data <- RHRV::BuildNIHR(e4_hrv_data)
  e4_hrv_data <- RHRV::FilterNIHR(e4_hrv_data)
  
  n_beats_accepted <- nrow(e4_hrv_data$Beat)
  
  e4_hrv_data <- RHRV::InterpolateNIHR(e4_hrv_data, freqhr = 4)
  e4_hrv_data <- RHRV::CreateTimeAnalysis(e4_hrv_data,size = 300,interval = 7.8125)
  
  
  e4_hrv_data <- RHRV::CreateFreqAnalysis(e4_hrv_data)
  
  e4_hrv_data <- RHRV::CalculatePowerBand(e4_hrv_data,
                                          indexFreqAnalysis = 1,
                                          size = 300, shift = 30, type = "fourier",
                                          ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                                          LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4
  )

  list(time = e4_hrv_data$TimeAnalysis[[1]],
       freq = e4_hrv_data$FreqAnalysis[[1]],
       n_beats_accepted = n_beats_accepted)
}, error = handle_hrv_error) 
  
# Assign time, freq and n_beats_accepted

# Assign time and freq
time <- result_hrv_processing$time
freq <- result_hrv_processing$freq
n_beats_accepted <- result_hrv_processing$n_beats_accepted

# The "as.numeric(NA)" solution does not work, because CreateTimeAnalysis throws an error after that. And the NA
# version also does not work.And "NA also does not work.
# e4_hrv_data$Beat$Time <- NULL
# e4_hrv_data$Beat$niHR <- NULL
# e4_hrv_data$Beat$RR <- NULL
# Result --> If $Beat is completely empty then it will throw an error as well.

# This is where it goes wrong.
# I need some error handling in the case that e4_hrv_data$Beat is 1 or 0
# What happens if Beat = 0?

### Solution###
# If this throws an error then set the time and frequency analyses to NA
# e4_hrv_data$TimeAnalysis[[1]] <- lapply(e4_hrv_data$TimeAnalysis[[1]], function(x) as.numeric(NA))
# e4_hrv_data$FreqAnalysis[[1]] <- lapply(e4_hrv_data$FreqAnalysis[[1]], function(x) as.numeric(NA))
# Wait there is another point to consider. If the function BuildNIHR or Interpolate do not work, then it
# is likely that Create Time Analysis does not work either. Check that.
# Result --> No it does not, so then I hae to set time and freq to NA






