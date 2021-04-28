## Resubmission

- 0.3.6 -> 0.3.7: Resubmitted to remove life stage badge link (fix NOTE)
- 0.3.5 -> 0.3.6: Resubmitted to remove bindings in base environment and its namespace while testing


## Test environments

- Windows 10    : local         : R 4.0.5
- Windows       : winbuilder    : devtools::check_win_release, devtools::check_win_devel
- Windows       : rhub          : R-devel, R-release
- Ubuntu        : travis-ci     : R-oldrel, R-release, R-devel
- Ubuntu        : rhub          : R-release (R-devel broken)
- macOS 10.13.6 : rhub          : R-release


## R CMD check results

- 0 errors | 0 warnings | 0 notes


## Downstream dependencies

- no downstream dependencies on CRAN
- checked with revdepcheck::revdep_check
