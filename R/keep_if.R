#' Performs Set Intersection
#'
#' Unlike intersect does not remove duplicates in x and keeps its order
#' @param  x,y Vectors (of the same mode) containing a sequence of items
#' @return A vector of the same mode as x or y
#' @examples
#' keep_if_in(1:5, 3:6)
#' @export
keep_if_in <- function(x, y) {

    x[x %in% y]
}

#' Discards The Elements Present In A Set And Not Present In The Other
#'
#' Unlike setdiff does not remove duplicates in x and keeps its order
#' @param  x,y Vectors (of the same mode) containing a sequence of items
#' @return A vector of the same mode as x or y
#' @examples
#' keep_if_not_in(1:5, 3:6)
#' @export
keep_if_not_in <- function(x, y) {

    x[!(x %in% y)]
}
