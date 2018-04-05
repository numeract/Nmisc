#' Is it a POSIXct object?
#' 
#' @param x An R object.
#'
#' @seealso \code{\link[lubridate:is.POSIXt]{lubridate::is.POSIXct}}
#' 
#' @export
is.POSIXct <- function(x) inherits(x, "POSIXct")


#' Format Date and POSIXct
#' 
#' Converts Date and POSIXct objects to the format given as input.
#' 
#' @param x A Date or POSIXct object to be converted.
#' 
#' @param format A character string. The default format is 
#'   "\%Y-\%m-\%d" for Date and "\%Y-\%m-\%d \%H:\%M:\%S" for POSIXct.
#' 
#' @param usetz Logical. If TRUE, the time zone abbreviation is
#'   appended to the output. Applicable only if an POSIXct object.
#' 
#' @return A character string representing the formatted date.
#' 
#' @seealso \code{\link{format.Date}}, \code{\link{format.POSIXct}}
#' 
#' @examples
#' format_utc(Sys.time(), format = "%Y-%m-%d", usetz = FALSE)
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


#' Current time in UTC time zone
#' 
#' Returns a vector with the current date and time in the UTC time zone.
#' 
#' @param length Positive integer (scalar) indicating the length of the vector.
#' 
#' @return A POSIXct vector of size \code{length} with the \code{tzone} 
#'   atribute set to "UTC".
#' 
#' @seealso \code{\link{Sys.time}}, \code{\link[lubridate:now]{lubridate::now}}
#' 
#' @examples 
#' now_utc(0)
#' # returns "POSIXct of length 0"
#' 
#' @export
now_utc <- function(length = 1L) {
    
    len <- as.integer(length[1L])
    stopifnot(base::length(len) == 1L || len >= 0L)
    
    if (len == 0L) {
        as.POSIXct(character(), tz = 'UTC')
    } else {
        now <- Sys.time()
        attr(now, "tzone") <- 'UTC'
        rep(now, len)
    }
}
