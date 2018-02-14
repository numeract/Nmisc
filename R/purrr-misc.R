#' Keep or discard elements
#'  
#' @description
#' 
#'  \code{keep_at()} keeps only the elements from specific positions or
#'   columns, while \code{discard_at()} does the opposite.
#'   The functions are wrappers for \code{\link{keep}} and  \code{\link{discard}}.
#'   respectively.
#' 
#' @param .x A table of data.
#' @param .at Character (column names) or a numeric (positions).
#' 
#' @return A table of data.
#' 
#' @examples 
#' x <- c(1, 2, 3)
#' names(x) <- c("First", "Second", "Last")
#' # returns 2
#' keep_at(x, "Second")
#' 
#' x <- c(1, 2, 3)
#' discard_at(x, 1)
#' # returns (2, 3)
#' 
#' @seealso \code{\link{keep}}
#' @seealso \code{\link{discard}}
#' 
#' @export
keep_at <- function(.x, .at) {
    
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
    
    .p <- if (is.character(.at)) {
        names(.x) %in% .at
    } else if (is.numeric(.at)) {
        seq_along(.x) %in% as.integer(.at)
    } else {
        stop("`.at` must be character (names) or a numeric (positions)")
    }
    
    purrr::discard(.x, .p)
}
