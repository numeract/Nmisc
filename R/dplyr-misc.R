#' Pull out a single column using another column for names
#'
#' First verision, var must be character or numeric
#'
#' @export
pull_with_names <- function(.data, var = -1, name_col) {
    dplyr::pull(.data, var) %>%
        rlang::set_names(.data[[name_col]])
}
