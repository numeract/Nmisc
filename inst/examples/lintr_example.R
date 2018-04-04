# nolint start

# Problems

f <- function(foo, na.rm) { } # it requests snake case for na.rm

# Line length should not exceed 80 characters
print("This line should be affected by the style checker because it is a very long line")

# Absolute paths should not be used
file.exists("~/docs/foo")

# Assignments should use <- operator
x = 5  # not ok

# Closed curly should be on its own line
if (TRUE) {
    print("not ok")}

# Commas should leave spaces afterwards
paste0("one","two")

# Infix operator spacing should exist
x <- 5+3

# No tabs, only spaces should be allowed
        x <- 3

# Only one naming convention should work
onlyCamel <- 5
only_snake <- 6

# No spaces inside parentheses should be allowed
if ( TRUE) {}

# Space before parentheses, as well
if(TRUE) {}

# There should be no trailing blank spaces at the end of the file


# nolint end
