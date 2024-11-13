# wearables 0.11.1
2025-11-13

## Bug fixes
* Added extended check in `aggregate_nowatch_data` to not only check for the presence of activity (ACT) data, but also other common datasets like HR and TEMP. The check for ACT only caused too many false positives, as the ACT data can't be expected to always be present in exports.

# wearables 0.11.0
2024-04-18

## New features
* Added new function to read data from Nowatch: `read_nowatch()`. This function will return a list of dataframes with the data from the Nowatch. Both a zipfile and a folder are accepted as input. This function is intended for aggregated data, as the Nowatch does not provide raw data.
* Added reading of aggregated data from Embrace Plus by adding a `type` argument in `read_embrace_plus()`. This argument can be set to `raw` or `aggregated` to read the raw or aggregated data from the Embrace Plus.
* `read_embrace_plus()` gains additional argument `folder` to choose the folder where the data is stored. This prevents the user to have to navigate to compress the files before using this function. It is still possible to use a zip file as input with the `zipfile` argument.
* `aggregate_data()` and friends (`aggregate_e4_data()`, `aggregate_embrace_plus_data()`, `aggregate_nowatch()`) now have a `interval` argument. It defaults to `"1 min"`, but can be changed if desired.

# wearables 0.10.0
2024-02-27

## New features
* Added `read_and_process_embrace_plus()` to read and process data from the Embrace Plus. This aligns with the `read_and_process_e4()` function for the E4.
* Made general `rbind_data()` and `aggregate_data()` functions, which makes it easier to on-board other devices in the future. In the future, these functions will also be preferred to functions like `rbind_e4()` and `aggregate_e4_data()`. Currently these functions are wrappers around the more general functions, to ensure backwards compatibility.

## Improvements
* Updated `read_embrace_plus()` to make the outcome more consistent with the E4. Instead of the `timestamp` column, the column is now named `DateTime`, and also the columns called `values` are renamed to align with E4 (e.g. `TEMP` instead of `values`).

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