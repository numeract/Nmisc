# Nmisc
[![Travis build status](https://travis-ci.org/numeract/Nmisc.svg?branch=master)](https://travis-ci.org/numeract/Nmisc)

Miscellaneous R functions used at Numeract. 

The functions are expected to be used in other packages and projects.

The functions mostly extend base and tidyverse packages.


## Installation

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
- code style:
    + `style_script` and `check_style`
