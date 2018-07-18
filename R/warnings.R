#' Avoid repeated warnings
#' 
#' Clear warnings for production code.
#' 
#' @seealso \code{\link{warnings}}
#' 
#' @export
clear_warnings <- function() {

    assign("last.warning", NULL, envir = baseenv())
    warnings()
}
