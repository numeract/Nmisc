#' Returns the name of the Operating Sytem
#'
#' A simple wrapper arround \code{rappdirs:::get_os},
#'  allowing it to be exported.
#'
#' @return One of \code{"win"}, \code{"mac"}, \code{"unix"},
#'  \code{"Unknown OS"}.
#'
#' @export
get_os <- getFromNamespace("get_os", "rappdirs")
