#' @importFrom magrittr %>%
#' @importFrom purrr %||%
#' @importFrom rlang .data
NULL


# quiets concerns of R CMD check re: Namespaces in Imports field not imported
#' @importFrom rappdirs user_data_dir
NULL


# quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
