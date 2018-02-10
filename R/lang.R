#' Evaluate object if a call or returns the object
#'
#' @export
qval <- function(x, default = NULL, envir = parent.frame()) {
    
    val <- if (class(x)[1L] == 'call') {
        tryCatch({
            eval(x, envir = envir)
        }, error = function(e) {
            warning(e$message)
            NULL
        })
    } else {
        x
    }
    
    val %||% default
}
