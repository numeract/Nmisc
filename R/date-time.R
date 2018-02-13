#' Date / Time functions
#' 
#' Checks if the input of the function is a POXIXct object.
#'
#' @param x an R object.
#'
#' @return TRUE if the object is of class POSIXct
#' and FALSE otherwise.
#'
#' @seealso \code{\link{inherits}}
#'
#' @export
is_POSIXct <- function(x) inherits(x, "POSIXct")

#' Format Date and POSIXct
#' 
#' Converts Date and POSIXct objects to a given format.
#' 
#' @param x An object to be converted.
#' @param format A character string. The default for the 
#' format methods is "%Y-%m-%d %H:%M:%S" if any element has a time
#' component which is not midnight, and "%Y-%m-%d" otherwise.
#' If options("digits.secs") is set, up to the specified 
#' number of digits will be printed for seconds.
#' @param usetz ogical. Should the time zone abbreviation 
#' be appended to the output? This is used in printing times,
#' and more reliable than using "%Z".
#' 
#' @return character vectors representing the time.
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
