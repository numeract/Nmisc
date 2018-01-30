#' Performs R code styling
#'
#' Pretty-prints R code without changing the user's formatting intent
#'
#' @param  file_path
#'
#' @return Invisibly returns a data frame that indicates for
#' each file considered for styling whether or not it was
#' actually changed.
#'
#' @section Warning:
#' This function overwrites files
#' (if styling results in a change of the code to be formatted).
#' It is strongly suggested to only style files that are under
#' version control or to create a backup copy
#'
#' @examples
#' style_script("file_name.R")
#'
#' @export
style_script <- function(file_path) {

  tidy_style <- styler::tidyverse_style(
    scope = "indention",
    strict = TRUE, indent_by = 2,
    start_comments_with_one_space = TRUE,
    reindention = styler::tidyverse_reindention(),
    math_token_spacing = styler::tidyverse_math_token_spacing()
  )
  styler::style_file(
    path = file_path,
    transformers = tidy_style
  )
}

