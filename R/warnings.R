#' Avoid R’s Warning Feature
#'
#' Clears warnings for production code
#' while keeping the last warning on base environment
#' @return  Does not return anything.
#' @seealso \code{\link{warning}}
#' @export
clear_warnings <- function() {

    assign("last.warning", NULL, envir = baseenv())
    warnings()
}
