#' Keep or discard elements
#'  
#' @description
#' 
#'  \code{keep_at()} keeps only the elements from specific positions or
#'   columns, while \code{discard_at()} does the opposite.
#'   The functions are wrappers around \code{purrr::keep} and 
#'   \code{purrr::discard}, respectively.
#' 
#' @param .x A list or vector.
#' @param .at Character (column names) or a numeric (positions).
#' 
#' @return A list or vector.
#' 
#' @examples 
#' x <- c(1, 2, 3)
#' names(x) <- c("First", "Second", "Last")
#' keep_at(x, "Second")
#' # returns 2
#' 
#' x <- c(1, 2, 3)
#' discard_at(x, 1)
#' # returns (2, 3)
#' 
#' @seealso \code{\link[purrr:keep]{purrr::keep}}
#' 
#' @export
keep_at <- function(.x, .at) {
    
    if (length(.at) == 0L) return(.x[0L])
    if (any(is.na(.at))) stop(".at should not contain NA's")
    .p <- if (is.character(.at)) {
        names(.x) %in% .at
    } else if (is.numeric(.at)) {
        seq_along(.x) %in% as.integer(.at)
    } else {
        stop("`.at` must be character (names) or a numeric (positions)")
    }
    
    purrr::keep(.x, .p)
}


#' @export
#' @rdname keep_at
discard_at <- function(.x, .at) {
    
    if (length(.at) == 0L) return(.x)
    if (any(is.na(.at))) stop(".at should not contain NA's")
    .p <- if (is.character(.at)) {
        names(.x) %in% .at
    } else if (is.numeric(.at)) {
        seq_along(.x) %in% as.integer(.at)
    } else {
        stop("`.at` must be character (names) or a numeric (positions)")
    }
    
    purrr::discard(.x, .p)
}
