#' Perform R code styling
#'
#' Pretty-print R code without changing the user's formatting intent.
#'
#' @param  file_path The path or the name of the file
#'   you want to style.
#'
#' @return Invisibly returns a data frame that indicates for
#'   each file considered for styling whether or not it was
#'   actually changed.
#'
#' @section Warning:
#'   This function overwrites files
#'   (if styling results in a change of the code to be formatted).
#'   It is strongly suggested to only style files that are under
#'   version control or to create a backup copy.
#'
#' @examples
#' \donttest{style_script("file_name.R")}
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


#' Check R code styling
#'
#' Check adherence to a given style, syntax errors and possible
#'   semantic issues.
#'
#' @param  file_name The name of the file you want to check.
#' 
#' @examples
#' \donttest{check_style("file_name.R")}
#'
#' @export
check_style <- function(file_name) {
    
    # lintr::with_defaults(camel_case_linter = NULL,
    #                     snake_case_linter = NULL)
    lintr::lint(file_name)
}
