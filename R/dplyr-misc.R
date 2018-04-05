
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
#' @examples pull_with_names(iris, 4, "Species")
#' 
#' @export
pull_with_names <- function(.data, var = -1, name_col) {
    if (is.null(name_col)) stop("name_col should be a character()")
    if (length(name_col) > 1) stop("name_col should be a character() of length 1") 
    if (is.na(name_col)) stop("name_col should be a character()")
    dplyr::pull(.data, var) %>%
        rlang::set_names(.data[[name_col]])
}

