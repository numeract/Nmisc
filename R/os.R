#' Returns the name of the Operating System
#' 
#' A simple wrapper around \code{rappdirs:::get_os},
#'  allowing it to be exported.
#' 
#' @return One of \code{"win"}, \code{"mac"}, \code{"unix"},
#'  \code{"Unknown OS"}.
#' 
#' @export
get_os <- utils::getFromNamespace("get_os", ns = "rappdirs")
