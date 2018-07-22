#' Pull out a single column
#' 
#' Pull out a single column by using its name or its position
#'   and name the obtained vector using values from another column.
#' 
#' @param .data	 A data frame
#' @param var 	The name of the column of interest,
#'   or a positive integer, giving the position counting from the left,
#'   or a negative integer, giving the position counting from the right.
#' @param name_col The column whose values will be used to
#'   name the pulled column.
#' 
#' @return A named vector.
#' 
#' @examples head(pull_with_names(iris, 4, "Species"))
#' 
#' @export
pull_with_names <- function(.data, var = -1, name_col) {
    
    if (!rlang::is_scalar_character(name_col) || is.na(name_col)) {
        stop("`name_col` must be a valid character of length 1")
    }
    
    dplyr::pull(.data, var) %>%
        rlang::set_names(.data[[name_col]])
}
