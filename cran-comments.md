## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Test environments
* local OS Win10 Pro install, R 4.1.0: OK

* travis-ci Ubuntu Xenial 16.04: OK

* check_win_release() and check_win_devel() --> 1 note:

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
    
* r-hub check
Ubuntu en Fedora checks are OK, 

Windows server 2008: one error on Bioconductor does not yet build and check packages for R version 4.2; see https://bioconductor.org. This issue is already opened on rhub:
https://github.com/r-hub/rhub/issues/471 

## This is the first submission of the package.
