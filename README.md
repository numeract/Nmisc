# Nmisc
[![Travis build status](https://travis-ci.org/numeract/Nmisc.svg?branch=master)](https://travis-ci.org/numeract/Nmisc)
[![Coverage status](https://codecov.io/gh/numeract/Nmisc/branch/master/graph/badge.svg)](https://codecov.io/github/numeract/Nmisc?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/Nmisc)](https://cran.r-project.org/package=Nmisc)


Contains functions useful for debugging, set operations on vectors,
and 'UTC' date and time functionality. It adds a few vector manipulation 
verbs to 'purrr' and 'dplyr' packages. It can also generate an R file to 
install and update packages to simplify deployment into production. The 
functions were developed at the data science firm 'Numeract LLC' and are 
used in several packages and projects.


## Installation

### From CRAN

```
install.packages("Nmisc")
```

### From GitHub

```
# install.packages("devtools")
devtools::install_github("numeract/Nmisc")
```

**Please note that the GitHub version requires GitHub versions of `glue` and `tidyselect`**


## Functions

The package is still in development, functions will be updated.

- related to `base` package
    + `catn`, `str1`, `str2`, `clear_warnings`
    + `keep_if_in` / `%if_in%`,  `keep_if_not_in` / `%if_not_in%`
    + `seq_ncol`, `seq_nrow` 
    + `setequal_na`
- date & time:
    + `is.POSIXct`, `format_utc`, `now_utc`
- os:
    + `get_os`
- extending `purrr`:
    + `keep_at` and `discard_at`
- extending `dplyr`:
    + `pull_with_names`
- package related:
    + `get_packages` and `generate_install_file`
