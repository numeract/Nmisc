#' Pull out a single column using another column for names
#'
#' First version, var must be character or numeric
#' 
#' @param .data	 A table of data
#' @param var 	A variable specified as: a literal variable name, 
#' a positive integer, giving the position counting from the left, 
#' a negative integer, giving the position counting from the right.
#' @param name_col The name of the newly obtained column
#' 
#' @return the new table of data
#' 
#' @export
pull_with_names <- function(.data, var = -1, name_col) {
    
    dplyr::pull(.data, var) %>%
        rlang::set_names(.data[[name_col]])
}
