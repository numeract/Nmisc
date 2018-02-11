#' Performs Set Intersection
#'
#' Unlike intersect does not remove duplicates in x and keeps its order
#'
#' @param  x,y Vectors (of the same mode) containing a sequence of items
#'
#' @return A vector of the same mode as x or y
#'
#' @examples
#' keep_if_in(1:5, 3:6)
#'
#' @export
keep_if_in <- function(x, y) {

    x[x %in% y]
}

#' Discards The Elements Present In A Set And Not Present In The Other
#'
#' Unlike setdiff does not remove duplicates in x and keeps its order
#'
#' @param  x,y Vectors (of the same mode) containing a sequence of items
#'
#' @return A vector of the same mode as x or y
#'
#' @examples
#' keep_if_not_in(1:5, 3:6)
#'
#' @export
keep_if_not_in <- function(x, y) {

    x[!(x %in% y)]
}


#' Performs Set Intersection
#'
#' Unlike intersect does not remove duplicates in x and keeps its order
#'
#' @export
`%if_in%` <- keep_if_in;


#' Discards The Elements Present In A Set And Not Present In The Other
#'
#' Unlike setdiff does not remove duplicates in x and keeps its order
#'
#' @export
`%if_not_in%` <- keep_if_not_in;


#' @export
setequal_na <- function(x, y, na.rm = FALSE) {
    
    if (na.rm) {
        x <- stats::na.omit(x)
        y <- stats::na.omit(y)
    }
    
    base::setequal(x, y)
}
