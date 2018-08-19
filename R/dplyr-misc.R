#' Pull out a single column
#' 
#' Pull out a single column by using its name or its position
#'   and name the obtained vector using values from another column.
#' 
#' @param .data	 A data frame
#' @param var 	The name of the column of interest,
#'   or a positive integer, giving the position counting from the left,
#'   or a negative integer, giving the position counting from the right.
#'   This argument supports tidyeval.
#' @param name_col The column whose values will be used to
#'   name the pulled column. This argument supports tidyeval.
#' 
#' @return A named vector.
#' 
#' @examples head(pull_with_names(iris, 4, "Species"))
#' 
#' @export
pull_with_names <- function(.data, var = -1, name_col) {
    
    var <- rlang::enquo(var)
    name_col <- rlang::enquo(name_col)
    
    x <- dplyr::pull(.data, !! var)
    nm <- dplyr::pull(.data, !! name_col)
    rlang::set_names(x, nm)
}
