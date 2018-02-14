#' Check if an object is POSIXct
#' 
#' Checks if the input of the function is a POXIXct object.
#'  It is a wrapper around inherits() function.
#'
#' @param x A R object.
#'
#' @return Logical. \code{TRUE} if the object is of class POSIXct
#'  and FALSE otherwise.
#' 
#' @examples 
#' is_POSIXct(lubridate::ymd_hms("2018/02/13 12-55-51"))
#' 
#' @seealso \code{\link{inherits}}
#'
#' @export
is_POSIXct <- function(x) inherits(x, "POSIXct")

#' Format Date and POSIXct
#' 
#' Converts Date and POSIXct objects to the format given as input.
#' 
#' @param x A Date or POSIXct object to be converted.
#' 
#' @param format A character string. The default format is 
#'   \"\%Y-\%m-\%d\" for Date and \"\%Y-\%m-\%d \%H:\%M:\%S\" for POSIXct.
#' 
#' @param usetz Logical. If TRUE, the time zone abbreviation is
#'  appended to the output.
#' 
#' @return A character string representing the formatted date.
#' 
#' @seealso \code{\link{format.Date}}
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
