#' Concatenate with Newline
#'
#' Wrapper to cat which appends new line to output.
#'
#' @param ... Arguments to be passed to `\code{\link[base]{cat}} function.
#'
#' @return None
#'
#' @seealso \code{\link{cat}}
#'
#' @export
catn <- function(...) cat(..., '\n')

#' Compactly display the structure of an arbitrary R object
#'
#' Wrapper to str which sets maximal level of nesting to 1.
#'
#' @param x Any R object about which you want to have some information.
#'
#' @return  Does not return anything.
#' The obvious side effect is output to the terminal.
#'
#' @seealso \code{\link{str}}
#'
#' @export
str1 <- function(x) utils::str(x,  max.level = 1)

#' Compactly Display The Structure Of An Arbitrary R Object
#'
#' Wrapper to str which sets maximal level of nesting to 2.
#'
#' @param x any R object about which you want to have some information
#'
#' @return  Does not return anything.
#' The obvious side effect is output to the terminal.
#'
#' @seealso \code{\link{str}}
#'
#' @export
str2 <- function(x) utils::str(x,  max.level = 2)
