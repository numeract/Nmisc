#' Keep elements present in x and not contained in y
#' 
#' Unlike \code{\link[base:sets]{intersect}}, it does not remove duplicates in
#'   x and keeps its order.
#' 
#' @param x  Source vector.
#' @param y  Destination vector (of the same mode as x).
#' 
#' @return A filtered version of x.
#' 
#' @examples
#' keep_if_in(1:5, 3:6)
#' # returns [3, 4, 5]
#' 
#' keep_if_in(c(4, 3, 4, 3, 1), 3:6)
#' # returns [4 3 4 3]
#' 
#' @seealso \code{\link{keep_if_not_in}}
#' 
#' @export
keep_if_in <- function(x, y) {
    
    x[x %in% y]
}


#' @rdname keep_if_in
#' @export
`%if_in%` <- keep_if_in;


#' Discard elements present in x and not contained in y
#' 
#' Unlike \code{\link[base:sets]{setdiff}}, it does not remove duplicates in x
#'   and keeps its order.
#' 
#' @inheritParams keep_if_in
#' 
#' @return A filtered version of x.
#' 
#' @examples
#' keep_if_not_in(1:5, 3:6)
#' # returns [1 2]
#' 
#' keep_if_not_in(c(4, 3, 4, 3, 1), 3:6)
#' # returns [1]
#' 
#' @seealso \code{\link{keep_if_in}}
#' 
#' @export
keep_if_not_in <- function(x, y) {
    
    x[!(x %in% y)]
}


#' @rdname keep_if_not_in
#' @export
`%if_not_in%` <- keep_if_not_in;


#' Check if two vectors have the same elements
#' 
#' Wrapper around \code{\link[base:sets]{setequal}} that adds extra parameter
#'   \code{na.rm}.
#' 
#' @param x,y Vectors (of the same mode) containing a sequence of items.
#' @param na.rm Boolean value indicating whether
#'  \code{NA} should be omitted or not.
#' 
#' @return A logical scalar that states the result.
#' 
#' @examples
#' setequal_na(c(2, 1, 3), c(1, 2, 3))
#' # returns TRUE
#' 
#' setequal_na(c(1, NA, 3), c(3, NA, 1), na.rm = TRUE)
#' # returns TRUE
#' 
#' setequal_na(c(NA, NA), c(NA), na.rm = TRUE)
#' # returns TRUE
#' 
#' setequal_na(c(NA, NA), c(NA))
#' # returns TRUE
#' 
#' setequal_na(c(1, 2, 3), c(1, 2, 3, NA))
#' # returns FALSE
#' 
#' @export
setequal_na <- function(x, y, na.rm = FALSE) {
    
    if (is.null(x) || is.null(y)) stop("Input sets should not be NULL")
    if (na.rm) {
        x <- stats::na.omit(x)
        y <- stats::na.omit(y)
    }
    
    base::setequal(x, y)
}
