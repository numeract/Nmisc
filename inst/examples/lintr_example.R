check_style(file.path("examples", "lintr_example.R"))

# Problems

f <- function(foo, na.rm) { } # it requests snake case for na.rm
    
if (TRUE) {
    # leading whitespace is still treated as trailing whitespace
    
    print("not ok")
}

# Line length
print("This line should be affected by the style checker because it is a very long line")

# Absolute path
file.exists("~/docs/foo")

# Assignment
x = 5  # not ok

# Closed curly
if (TRUE) { 
    print("not ok") }

# Commas
paste0("one","two")

# Infix space
x <- 5+3

# No tabs
        x <- 3

# Object name linter
onlySnake <- 5
names_please <- 6

# No spaces inside parentheses
if ( TRUE) {}

# Space before parentheses, as well
if(TRUE) {}

# And the trailing blank spaces at the end of the file

