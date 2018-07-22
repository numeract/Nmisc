## Test environments

- Windows 10 on local R 3.5.1, winbuilder R-release, R-devel
- Ubuntu 14.04.05 on travis-ci: R 3.4.4, 3.5.0, R-devel
- Ubuntu 16.04 on r-hub builder: R-release
- macOS 10.11 El Capitan on r-hub builder: R-release


## R CMD check results

- 0 errors | 0 warnings | 1 note
- this is the first submission


## Downstream dependencies

- no downstream dependencies on CRAN


## Resubmit comments

- updated Authors@R field in DESCRIPTION file (thank you)
- updated tests for generate_install_file() to write only to a temp file;
  generate_install_file examples use \dontrun; there are no vignettes
