#' Creates A Sequence Based On Data Frame Number Of Rows
#'
#' Creates a sequence that starts at 1 and with steps of 1
#' which finishes at the number value corresponding to the
#' number of rows in a data frame given as input.
#' @param df a data frame
#' @return a vector of integers
#' @seealso \code{\link{seq}}
#' @seealso \code{\link{seq_len}}
#' @export
seq_nrow <- function(df) seq_len(nrow(df))

#' Creates A Sequence Based On Data Frame Number Of Columns
#'
#' Creates a sequence that starts at 1 and with steps of 1
#' which finishes at the number value corresponding to the
#' number of columns in a data frame given as input.
#' @param df a data frame
#' @return a vector of integers
#' @seealso \code{\link{seq}}
#' @seealso \code{\link{seq_along}}
#' @export
seq_ncol <- function(df) seq_along(df)