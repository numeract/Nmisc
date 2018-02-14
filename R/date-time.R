#' Date / Time functions
#' 
#' Checks if the input of the function is a POXIXct object.
#'
#' @param x an R object.
#'
#' @return TRUE if the object is of class POSIXct
#' and FALSE otherwise.
#'
#' @export
is_POSIXct <- function(x) inherits(x, "POSIXct")

#' Format Date and POSIXct
#' 
#' Converts Date and POSIXct objects to a given format.
#' 
#' @param x An object to be converted.
#' 
#' @param format A character string. The default for the  format methods is 
#'   \"\%Y-\%m-\%d \%H:\%M:\%S\" for POSIXct and \"\%Y-\%m-\%d\" for Date.
#' 
#' @param usetz logical. Should the time zone abbreviation 
#'   be appended to the output?
#' 
#' @return character vectors representing the time.
#' 
#' @seealso \code{\link{format.POSIXct}}
#' 
#' @export
format_utc <- function(x, format = NULL, usetz = TRUE) {
    
    if (inherits(x, "Date")) {
        if (is.null(format)) {
            format <- "%Y-%m-%d"
        }
        format.Date(x, format = format)
    } else if (inherits(x, "POSIXct")) {
        if (is.null(format)) {
            format <- "%Y-%m-%d %H:%M:%S"
        }
        format.POSIXct(x, format = format, tz = 'UTC', usetz = usetz)
    } else {
        stop("not a Date/POSIXct")
    }
}
