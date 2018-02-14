#' Creates a sequence based on the number of rows or columns
#'
#' Creates a sequence from 1 to the number of row or columns, respectively.
#'
#' @param x a data frame or a matrix
#'
#' @return a vector of integers
#'
#' @seealso \code{\link{seq}}
#' 
#' @export
seq_nrow <- function(x) seq_len(nrow(x))


#' @export
#' @rdname seq_nrow
seq_ncol <- function(x) seq_along(x)
