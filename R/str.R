#' Concatenate with new line
#' 
#' Wrapper around cat which appends new line to output.
#' 
#' @param ... Arguments to be passed to \code{\link[base]{cat}} function.
#' 
#' @return None
#' 
#' @seealso \code{\link{cat}}
#' 
#' @export
catn <- function(...) cat(..., "\n") # nocov


#' High level overview of the structure of an R object
#' 
#' \code{str1()} is a wrapper around \code{\link{str}} which sets maximal level
#'   of nesting to 1, while \code{str2()} sets maximal level of nesting to 2.
#' 
#' @param x An R object
#' 
#' @return  Does not return anything.
#' 
#' @seealso \code{\link{str}}
#' 
#' @export
str1 <- function(x) utils::str(x, max.level = 1) # nocov


#' @export
#' @rdname str1
str2 <- function(x) utils::str(x, max.level = 2) # nocov
