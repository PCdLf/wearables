# wearables 0.9.0
2024-01-30

## New features
* Added a function to read in data from the Empatica Embrace Plus: `read_embrace_plus()`. This function will return a list of dataframes with the data from the Embrace Plus.

# wearables 0.8.3
2024-01-12

This is the fourth release of a package that contains tools to Read and Convert Wearables Data.

## New features 
* Added functions to handle empty IBI files for the Empatica E4.
* `ibi_analysis()` New version of ibi_analysis function: This function is an updated approach for the analysis of (IBI) data.
* Fixed a note on the join_eda_bin function.

# wearables 0.8.2
2022-03-01

* Added a function to cut files in intervals over a time period. 
* Set the threshold for peak detection to .005 in accordance with de Looff et al. (2019)
* Updated function join_eda_bin, sometimes the quality_flag received NA if the function
did not floor the date properly