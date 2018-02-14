#' Avoid R’s warning feature
#'
#' Clear warnings for production code.
#'
#' @seealso \code{\link{warning}}
#'
#' @export
clear_warnings <- function() {

    assign("last.warning", NULL, envir = baseenv())
    warnings()
}
