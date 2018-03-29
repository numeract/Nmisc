library(dplyr)
library(lintr)


# overwrite .lintr based on one file
if (FALSE) {
    lintr::lint('<name of test file>') %>%
        as.data.frame %>%
        group_by(linter) %>%
        tally(sort = TRUE) %$%
        sprintf("linters: with_defaults(\n    %s\n    NULL\n  )\n",
                paste0(linter, " = NULL, # ", n, collapse = "\n    ")) %>%
        cat(file = ".lintr")
}

lintr::lint('constants.R')
lintr::lint('helper.R')
