## Overview
### wearables
Functions for analyzing empatica e4 data, pre-process the signals, detect artifacts
and create several features for analysis.

We are working on functionality to also process Empatica EmbracePlus and Nowatch data.
These devices can be found here: 

https://www.empatica.com/embraceplus/
https://nowatch.com/

This package was originally created for signal analysis of the Empatica E4 wearables device. It allows
users to read in an E4 zip file from Empatica connect into a list.
The package was created to detect artifacts and extract features that can be used for 
analysis.

- 'read_e4()' is the first function that can be used to read Empatica E4 data into a list.
- 'read_and_process()' is a function to perform analyses on the heart rate, electrodermal activity, accelerometry and temperature data.

If you are new to Empatica E4, the best place to start is the [website from Empatica ](https://www.empatica.com/research/e4/) or the accompanying website for the [Shiny tool
](https://pcdlf.shinyapps.io/e4dashboard/).

[![R-CMD-check](https://github.com/PCdLf/wearables/workflows/R-CMD-check/badge.svg)](https://github.com/PCdLf/wearables/actions)

## Installation

``` r
#install the wearables package:
install.packages("wearables")
```

## Examples

``` r
library(wearables)

#read_e4("Your filepath to zip-file here")
#read_and_process_e4("Your filepath to zip-file here")

```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/PCdLf/wearables/issues). 