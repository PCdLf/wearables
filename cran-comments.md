## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Test environments
* local OS Win10 Pro install, R 4.1.0: OK
* travis-ci : OK
* r-hub check wearables 0.6.2: OK
* devtools::check_win_release() --> 1 note

Possibly mis-spelled words in DESCRIPTION:
  Empatica (18:21)
  Wearables (4:34)

###These words are spelled correctly

Found the following (possibly) invalid URLs:
  URL: https://eda-explorer.media.mit.edu/
    From: man/binary_classifier_config.Rd
          man/multiclass_classifier_config.Rd
    Status: Error
    Message: SSL certificate problem: certificate has expired

###The website exists, but the SSL certificate has expired. Authors of the website are notified, this will have no effect on the package.


## This is the first submission of the package.
