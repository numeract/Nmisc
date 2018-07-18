# Nmisc
[![Travis build status](https://travis-ci.org/numeract/Nmisc.svg?branch=master)](https://travis-ci.org/numeract/Nmisc)
[![Coverage status](https://codecov.io/gh/numeract/Nmisc/branch/master/graph/badge.svg)](https://codecov.io/github/numeract/Nmisc?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/Nmisc)](https://cran.r-project.org/package=Nmisc)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)


Miscellaneous R functions used at Numeract. 

The functions are expected to be used in other packages and projects.

The functions mostly extend base and tidyverse packages.


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
