#' Avoid R’s Warning Feature
#'
#' Clears warnings for production code
#'
#' @return  Does not return anything.
#'
#' @seealso \code{\link{warning}}
#'
#' @export
clear_warnings <- function() {

    assign("last.warning", NULL, envir = baseenv())
    warnings()
}
