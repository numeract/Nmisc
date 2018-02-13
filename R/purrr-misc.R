#' Keep elements using their names
#'
#' @param .x a table of data
#' @param .at character (names) or a numeric (positions)
#' 
#' @return a table of data 
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


#' Discard elements using their names
#'
#' @param .x a table of data
#' @param .at character (names) or a numeric (positions)
#' 
#' @return a table of data
#' 
#' @export
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
