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
    
    if (!file.exists(file_path)) 
        stop(paste0("Cannot find file: ", file_path,
                    ". Did you specify the right path?"))
    
    tidy_style <- styler::tidyverse_style(
        scope = "tokens",
        strict = TRUE, indent_by = 4,
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
#' @param file_path The path to the file you want to check.
#' 
#' @examples
#' \donttest{check_style("file_name.R")}
#'
#' @export
check_style <- function(file_path) {
    
    linters <- list(
        line_length_linter = lintr::line_length_linter(80),
        absolute_paths_linter = lintr::absolute_paths_linter,
        assignment_linter = lintr::assignment_linter,
        closed_curly_linter = lintr::closed_curly_linter,
        commas_linter = lintr::commas_linter,
        infix_spaces_linter = lintr::infix_spaces_linter,
        no_tab_linter = lintr::no_tab_linter,
        multiple_dots_linter = lintr::multiple_dots_linter,
        open_curly_linter = lintr::open_curly_linter,
        spaces_inside_linter = lintr::spaces_inside_linter,
        spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
        trailing_blank_lines_linter = lintr::trailing_blank_lines_linter)
    
    # lintr::lint_package(path = ".", linters = linters)
    
    if (!file.exists(file_path))
        stop(paste0("Cannot find file: ", file_path, 
                    ". Did you specify the right path?"))
    
    lintr::lint(file_path, linters = linters)
}
